{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (ConnectInfo)

import Types
import Database

createAccountHandler :: ConnectInfo -> ActionM ()
createAccountHandler db = do
  result <- liftIO $ withConnection db (createAccount "Test Name")
  case result of
    [newAccount] -> json newAccount
    _ -> error "what to do?"

bank :: ConnectInfo -> ScottyM ()
bank db = do
  get "/account/:id" $ do
    json $ AccountInfo 1 "Mr. Orange" 500
  post "/account" (createAccountHandler db)
  get "/echo/:word" $ do
    beam <- param "word"
    text beam
