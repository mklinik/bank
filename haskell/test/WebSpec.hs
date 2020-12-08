{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module WebSpec where

import Test.Hspec
import Test.Hspec.Wai
import qualified Web.Scotty as S
import Data.Aeson

import WaiUtil
import Types
import Web
import Database

app = S.scottyApp (bank testDB)

spec :: Spec
spec = with app $ do
  describe "/account" $ do
    it "creates an account" $ do
      get "/account/1" `shouldRespondWith` jsonBody
        [ "name" .= String "Mr. Orange"
        , "account-number" .= Number 1
        , "balance" .= Number 500
        ]
