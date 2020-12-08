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
