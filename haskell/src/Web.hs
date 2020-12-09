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

createAccountHandler :: WithConn IO DbResult -> ActionM ()
createAccountHandler withConn = do
  accountParams <- jsonData
  generateResponse =<< liftIO (withConn (createAccount (AP.name accountParams)))

getAccountHandler :: WithConn IO DbResult -> ActionM ()
getAccountHandler withConn = do
  accountNumber <- param "id"
  generateResponse =<< liftIO (withConn (getAccount accountNumber))

depositMoneyHandler :: WithConn IO DbResult -> ActionM ()
depositMoneyHandler withConn = do
  accountNumber <- param "id"
  depositParams <- jsonData
  generateResponse =<< liftIO (withConn $ depositMoney accountNumber (DP.amount depositParams))


withdrawMoneyHandler :: WithConn IO DbResult -> ActionM ()
withdrawMoneyHandler withConn = do
  accountNumber <- param "id"
  withdrawParams <- jsonData
  generateResponse =<< liftIO (withConn $ withdrawMoney accountNumber (DP.amount withdrawParams))


transferMoneyHandler :: WithConn IO DbResult -> ActionM ()
transferMoneyHandler withConn = do
  senderNumber <- param "id"
  transferParams <- jsonData
  let receiverNumber = TP.account_number transferParams
  let amount = TP.amount transferParams
  generateResponse =<< liftIO (withConn $ transferMoney senderNumber receiverNumber amount)


bank :: WithConn IO DbResult -> ScottyM ()
bank withConn = do
  get "/account/:id" (getAccountHandler withConn)
  post "/account" (createAccountHandler withConn)
  post "/account/:id/deposit" (depositMoneyHandler withConn)
  post "/account/:id/withdraw" (withdrawMoneyHandler withConn)
  post "/account/:id/transfer" (transferMoneyHandler withConn)
