{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (ConnectInfo)

import Types
import qualified Types.AccountParams as AP
import Database

createAccountHandler :: ConnectInfo -> ActionM ()
createAccountHandler db = do
  accountParams <- jsonData
  result <- liftIO $ withConnection db (createAccount (AP.name accountParams))
  case result of
    [newAccount] -> json newAccount
    _ -> error "TODO: generate error response"


getAccountHandler :: ConnectInfo -> ActionM ()
getAccountHandler db = do
  account_number <- param "id"
  result <- liftIO $ withConnection db (getAccount account_number)
  case result of
    [gotAccount] -> json gotAccount
    _ -> error "TODO: generate error response"


bank :: ConnectInfo -> ScottyM ()
bank db = do
  get "/account/:id" (getAccountHandler db)
  post "/account" (createAccountHandler db)
