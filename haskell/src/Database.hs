{-# LANGUAGE OverloadedStrings #-}
module Database where

import Types
import Database.PostgreSQL.Simple
  ( ConnectInfo
  , Connection
  , Only (..)
  , connectHost
  , connectDatabase
  , connectUser
  , connectPassword
  )
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.Transaction as SQL
import Data.Functor
import Data.ByteString (ByteString)
import Control.Exception.Safe
import Control.Monad
import Data.Text.Lazy (Text)


data DbResult
  = Success AccountInfo
  | ENoSuchAccount
  | EOther Text
  deriving (Show, Eq)

mkDbResult :: [AccountInfo] -> DbResult
mkDbResult [gotAccount] = Success gotAccount
mkDbResult _ = ENoSuchAccount


productionDB = SQL.defaultConnectInfo
  { connectHost = "localhost"
  , connectDatabase = "bank-hs"
  , connectUser = "mkl"
  , connectPassword = "w00t"
  }

testDB = SQL.defaultConnectInfo
  { connectHost = "localhost"
  , connectDatabase = "bank-hs-test"
  , connectUser = "mkl"
  , connectPassword = "w00t"
  }


withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connInfo f = do
  bracket (SQL.connect connInfo) SQL.close f


createTables :: Connection -> IO ()
createTables conn = void $ SQL.execute_ conn $ mconcat
  [ "create table if not exists account"
  , "( account_number serial not null"
  , ", name text not null"
  , ", balance bigint"
  , ", primary key (account_number)"
  , ")"
  ]


dropTables :: Connection -> IO ()
dropTables conn = void $ SQL.execute_ conn "drop table if exists account"


resetDatabase :: Connection -> IO ()
resetDatabase conn = do
  dropTables conn
  createTables conn


createAccount :: String -> Connection -> IO DbResult
createAccount name conn =
  mkDbResult <$> SQL.query conn "insert into account (name,balance) values (?, 0) returning *" (Only name)


getAccount :: Int -> Connection -> IO DbResult
getAccount accountNumber conn =
  mkDbResult <$> SQL.query conn "select * from account where account_number = ?" (Only accountNumber)


depositMoney :: Int -> Int -> Connection -> IO DbResult
depositMoney accountNumber amount conn =
  if (amount < 0)
    then return $ EOther "amount must be positive"
    else mkDbResult <$> SQL.query conn
      "update account set balance = balance + ? where account_number = ? returning *"
      (amount, accountNumber)


-- TODO: insufficient funds generates ENoSuchAccount, which is probably not what we want
withdrawMoney :: Int -> Int -> Connection -> IO DbResult
withdrawMoney accountNumber amount conn =
  if (amount < 0)
    then return $ EOther "amount must be positive"
    else mkDbResult <$> SQL.query conn
      (mconcat
        [ "update account set balance = balance - ?"
        , " where account_number = ?"
        , " and balance >= ?"
        , " returning *"
        ])
      (amount, accountNumber, amount)


transferMoney :: Int -> Int -> Int -> Connection -> IO DbResult
transferMoney senderNumber receiverNumber amount conn
  | amount < 0 = return $ EOther "amount must be positive"
  | senderNumber == receiverNumber = return $ EOther "sender must be different from receiver"
  | otherwise = SQL.withTransactionSerializable conn $ do
      mbSenderAccount <- getAccount senderNumber conn
      mbReceiverAccount <- getAccount receiverNumber conn
      case (mbSenderAccount, mbReceiverAccount) of
        (Success senderAccount, Success receiverAccount) ->
          if (balance senderAccount < amount)
            then return $ EOther "insufficient funds"
            else do
              SQL.execute conn
               "update account set balance = balance + ? where account_number = ?"
                (amount, receiverNumber)
              mkDbResult <$> SQL.query conn
                "update account set balance = balance - ? where account_number = ? returning *"
                (amount, senderNumber)
        _  -> return $ ENoSuchAccount
