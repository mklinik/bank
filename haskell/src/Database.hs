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

type WithConn m a = (Connection -> m a) -> m a

individualConnection :: ConnectInfo -> WithConn IO a
individualConnection connInfo f = do
  bracket (SQL.connect connInfo) SQL.close (\conn -> (SQL.withTransactionSerializable conn) (f conn))


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


depositMoney :: Int -> Amount -> Connection -> IO DbResult
depositMoney accountNumber amount conn =
  mkDbResult <$> SQL.query conn
    "update account set balance = balance + ? where account_number = ? returning *"
    (getAmount amount, accountNumber)


-- TODO: insufficient funds generates ENoSuchAccount, which is probably not what we want
withdrawMoney :: Int -> Amount -> Connection -> IO DbResult
withdrawMoney accountNumber amount conn =
  mkDbResult <$> SQL.query conn
    (mconcat
      [ "update account set balance = balance - ?"
      , " where account_number = ?"
      , " and balance >= ?"
      , " returning *"
      ])
    (getAmount amount, accountNumber, getAmount amount)


transferMoney :: Int -> Int -> Amount -> Connection -> IO DbResult
transferMoney senderNumber receiverNumber amount conn
  | senderNumber == receiverNumber = return $ EOther "sender must be different from receiver"
  | otherwise = do
      mbSenderAccount <- getAccount senderNumber conn
      mbReceiverAccount <- getAccount receiverNumber conn
      case (mbSenderAccount, mbReceiverAccount) of
        (Success senderAccount, Success receiverAccount)
          | balance senderAccount < getAmount amount -> return $ EOther "insufficient funds"
          | otherwise -> do
              SQL.execute conn
               "update account set balance = balance + ? where account_number = ?"
                (getAmount amount, receiverNumber)
              mkDbResult <$> SQL.query conn
                "update account set balance = balance - ? where account_number = ? returning *"
                (getAmount amount, senderNumber)
        _ -> return ENoSuchAccount
