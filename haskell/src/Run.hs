{-# LANGUAGE OverloadedStrings #-}
module Run where

import Web.Scotty
import Database.PostgreSQL.Simple

import Web
import Database

run :: IO ()
run = myWebApp productionDB

myWebApp db = do
  individualConnection db createTables
  scotty 3000 (bank (individualConnection db))

hello :: IO ()
hello = do
  conn <- connect (defaultConnectInfo {connectDatabase = "bank-hs", connectUser = "mkl", connectPassword = "w00t"})
  [Only i] <- query conn "select 7 + ?" (Only (5 :: Int))
  print (i :: Int)
