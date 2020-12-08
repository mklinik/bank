{-# LANGUAGE OverloadedStrings #-}
module Database where

import Types
import Database.PostgreSQL.Simple
import Data.Functor
import Data.ByteString (ByteString)

myConnectInfo =  defaultConnectInfo
  { connectHost = "localhost"
  , connectDatabase = "bank-hs"
  , connectUser = "mkl"
  , connectPassword = "w00t"
  }


withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connInfo f = do
  conn <- connect connInfo
  result <- f conn
  close conn
  return result


createTables :: Connection -> IO ()
createTables conn = void $ execute_ conn $ mconcat
  [ "create table if not exists account"
  , "( account_number serial not null"
  , ", name text not null"
  , ", balance bigint"
  , ", primary key (account_number)"
  , ")"
  ]


dropTables :: Connection -> IO ()
dropTables conn = void $ execute_ conn "drop table if exists account"


reset :: Connection -> IO ()
reset conn = do
  dropTables conn
  createTables conn


createAccount :: String -> Connection -> IO [AccountInfo]
createAccount name conn =
  query conn "insert into account (name,balance) values (?, 0) returning *" (Only name)


getAccount :: Int -> Connection -> IO [AccountInfo]
getAccount accountNumber conn =
  query conn "select * from account where account_number = ?" (Only accountNumber)
