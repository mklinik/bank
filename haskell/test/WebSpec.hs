{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module WebSpec where

import Test.Hspec
import Test.Hspec.Wai
import qualified Web.Scotty as S
import Data.Aeson
import Data.Yaml.TH

import TestUtil
import Types
import Web
import Database

app = S.scottyApp (bank testDB)

spec :: Spec
spec = with app $ before_ (withTestDB resetDatabase) $ do
  describe "/account" $ do
    it "creates an account" $ do
      post "/account" (encode [yamlQQ| name: Mr. Orange |]) `shouldRespondWith` jsonBody
        [yamlQQ|
          name: Mr. Orange
          account-number: 1
          balance: 0
        |]

    context "when called repeatedly" $ do
      it "creates multiple accounts" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |]) `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. Orange
            account-number: 1
            balance: 0 |]
        post "/account" (encode [yamlQQ| name: Mr. White |]) `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. White
            account-number: 2
            balance: 0 |]
        post "/account" (encode [yamlQQ| name: Mr. Black |]) `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. Black
            account-number: 3
            balance: 0 |]

    context "when there are no tables" $ do
      it "responds with a 500 status code" $ do
        liftIO $ withTestDB dropTables
        post "/account" (encode [yamlQQ| name: Mr. Black |]) `shouldRespondWith` 500

  describe "/account/:id" $ do
    it "retrieves an account" $ do
      post "/account" (encode [yamlQQ| name: Mr. Orange |])
      get "/account/1" `shouldRespondWith` jsonBody
        [yamlQQ|
          name: Mr. Orange
          account-number: 1
          balance: 0
        |]

    context "when multiple accounts exist" $ do
      it "can retrieves all accounts" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |])
        post "/account" (encode [yamlQQ| name: Mr. White |])
        post "/account" (encode [yamlQQ| name: Mr. Black |])
        get "/account/1" `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. Orange
            account-number: 1
            balance: 0
          |]
        get "/account/2" `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. White
            account-number: 2
            balance: 0
          |]
        get "/account/3" `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. Black
            account-number: 3
            balance: 0
          |]

    context "when trying to retreive a non-existing account" $ do
      it "returns a 404 error message" $ do
        get "/account/200" `shouldRespondWith` 404


  describe "/account/:id/deposit" $ do
    it "deposits money in the happy case" $ do
      post "/account" (encode [yamlQQ| name: Mr. Orange |])
      post "/account/1/deposit" (encode [yamlQQ| amount: 202 |])
        `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. Orange
            account-number: 1
            balance: 202
          |]

    context "when given an invalid account number" $ do
      it "returns an error status code" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |])
        post "/account/blah/deposit" (encode [yamlQQ| amount: 202 |])
          `shouldRespondWith` 404

    context "when given a non-existing account number" $ do
      it "returns an error status code" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |])
        post "/account/200/deposit" (encode [yamlQQ| amount: 202 |])
          `shouldRespondWith` 400


  describe "/account/:id/withdraw" $ do
    it "withdraws the given amount in the happy case" $ do
      post "/account" (encode [yamlQQ| name: Mr. Orange |])
      post "/account/1/deposit" (encode [yamlQQ| amount: 5000 |])
      post "/account/1/withdraw" (encode [yamlQQ| amount: 300 |]) `shouldRespondWith` jsonBody
        [yamlQQ|
          name: Mr. Orange
          account-number: 1
          balance: 4700
        |]

    context "when given a negative amount" $ do
      it "returns an error status code" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |])
        post "/account/1/deposit" (encode [yamlQQ| amount: 5000 |])
        post "/account/1/withdraw" (encode [yamlQQ| amount: -300 |]) `shouldRespondWith` 400

    context "when given a non-existent account number" $ do
      it "returns an error status code" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |])
        post "/account/1/deposit" (encode [yamlQQ| amount: 5000 |])
        post "/account/1000/withdraw" (encode [yamlQQ| amount: 300 |]) `shouldRespondWith` 400

    context "when given a malformed account number" $ do
      it "returns an error status code" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |])
        post "/account/1/deposit" (encode [yamlQQ| amount: 5000 |])
        post "/account/foobar/withdraw" (encode [yamlQQ| amount: 300 |]) `shouldRespondWith` 404


  describe "/account/:id/transfer" $ do
    it "transfers money in the happy case" $ do
        post "/account" (encode [yamlQQ| name: Mr. Orange |])
        post "/account" (encode [yamlQQ| name: Mr. Black |])
        post "/account/1/deposit" (encode [yamlQQ| amount: 5000 |])
        post "/account/1/transfer" (encode [yamlQQ|
          amount: 50
          account-number: 2 |])
          `shouldRespondWith` jsonBody
          [yamlQQ|
            name: Mr. Orange
            account-number: 1
            balance: 4950
          |]
