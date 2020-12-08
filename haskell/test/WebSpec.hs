{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module WebSpec where

import Test.Hspec
import Test.Hspec.Wai
import qualified Web.Scotty as S
import Data.Aeson
import Data.Yaml.TH

import WaiUtil
import Types
import Web
import Database

app = S.scottyApp (bank testDB)

spec :: Spec
spec = with app $ do
  describe "/account" $ do
    it "creates an account" $ do
      post "/account" (encode [yamlQQ| name: Mr. Orange |]) `shouldRespondWith` jsonBody
        [yamlQQ|
          name: Mr. Orange
          account-number: 1
          balance: 0
        |]
