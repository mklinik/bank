{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (ConnectInfo)
import Network.HTTP.Types.Status

import Types
import qualified Types.AccountParams as AP
import qualified Types.DepositParams as DP
import qualified Types.TransferParams as TP
import Database

generateResponse :: DbResult -> ActionM ()
generateResponse (Success gotAccount) = json gotAccount
generateResponse ENoSuchAccount = raiseStatus status404 "no such account"
generateResponse (EOther message) = raiseStatus status400 message

createAccountHandler :: ConnectInfo -> ActionM ()
createAccountHandler db = do
  accountParams <- jsonData
  result <- liftIO $ withConnection db (createAccount (AP.name accountParams))
  generateResponse result

getAccountHandler :: ConnectInfo -> ActionM ()
getAccountHandler db = do
  accountNumber <- param "id"
  result <- liftIO $ withConnection db (getAccount accountNumber)
  case result of
    [gotAccount] -> json gotAccount
    _ -> raiseStatus status404 "no such account"


depositMoneyHandler :: ConnectInfo -> ActionM ()
depositMoneyHandler db = do
  accountNumber <- param "id"
  depositParams <- jsonData
  result <- liftIO $ withConnection db $ depositMoney accountNumber (DP.amount depositParams)
  case result of
    [gotAccount] -> json gotAccount
    _ -> raiseStatus status400 "could not deposit"


withdrawMoneyHandler :: ConnectInfo -> ActionM ()
withdrawMoneyHandler db = do
  accountNumber <- param "id"
  withdrawParams <- jsonData
  result <- liftIO $ withConnection db $ withdrawMoney accountNumber (DP.amount withdrawParams)
  case result of
    [gotAccount] -> json gotAccount
    _ -> raiseStatus status400 "could not withdraw"


transferMoneyHandler :: ConnectInfo -> ActionM ()
transferMoneyHandler db = do
  senderNumber <- param "id"
  transferParams <- jsonData
  let receiverNumber = TP.account_number transferParams
  let amount = TP.amount transferParams
  result <- liftIO $ withConnection db $ transferMoney senderNumber receiverNumber amount
  case result of
    [gotAccount] -> json gotAccount
    _ -> raiseStatus status400 "could not transfer"


bank :: ConnectInfo -> ScottyM ()
bank db = do
  get "/account/:id" (getAccountHandler db)
  post "/account" (createAccountHandler db)
  post "/account/:id/deposit" (depositMoneyHandler db)
  post "/account/:id/withdraw" (withdrawMoneyHandler db)
  post "/account/:id/transfer" (transferMoneyHandler db)
