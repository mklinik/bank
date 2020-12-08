{-# LANGUAGE OverloadedStrings #-}
module Run where

import Web.Scotty
import Database.PostgreSQL.Simple

import Web
import Database

run :: IO ()
run = myWebApp productionDB

myWebApp db = scotty 3000 (bank db)

hello :: IO ()
hello = do
  conn <- connect (defaultConnectInfo {connectDatabase = "bank-hs", connectUser = "mkl", connectPassword = "w00t"})
  [Only i] <- query conn "select 7 + ?" (Only (5 :: Int))
  print (i :: Int)
