{-# LANGUAGE OverloadedStrings #-}
module Run where

import Web.Scotty
import Database.PostgreSQL.Simple

import Web

run :: IO ()
run = hello

myWebApp = scotty 3000 bank

hello :: IO ()
hello = do
  conn <- connect (defaultConnectInfo {connectDatabase = "bank-hs", connectUser = "mkl", connectPassword = "w00t"})
  [Only i] <- query conn "select 2 + ?" (Only (5 :: Int))
  print (i :: Int)
