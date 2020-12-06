{-# LANGUAGE OverloadedStrings #-}

module WebSpec where

import Test.Hspec
import Network.Wai.Test
import Web.Scotty (scottyApp)
import Data.Aeson

import Types
import Web

spec :: Spec
spec = do
  describe "account" $ do
    it "creates an account" $ do
      bankApp <- scottyApp bank
      resp <- runSession (request $ setPath defaultRequest "/account/500") bankApp
      simpleBody resp `shouldBe` encode (AccountInfo 1 "Mr. Orange" 500)
