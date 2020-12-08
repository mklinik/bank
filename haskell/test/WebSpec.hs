{-# LANGUAGE OverloadedStrings #-}
module WebSpec where

import Test.Hspec
import Network.Wai.Test
import Web.Scotty (scottyApp)
import Data.Aeson

import Types
import Web
import Database

spec :: Spec
spec = do
  describe "echo" $ do
    it "echos a word" $ do
      bankApp <- scottyApp (bank testDB)
      resp <- runSession (request $ setPath defaultRequest "/echo/foobar") bankApp
      simpleBody resp `shouldBe` "foobar"
  describe "account" $ do
    it "creates an account" $ do
      bankApp <- scottyApp (bank testDB)
      resp <- runSession (request $ setPath defaultRequest "/account/500") bankApp
      decode (simpleBody resp) `shouldBe` Just (AccountInfo 1 "Mr. Orange" 500)
