{-# LANGUAGE OverloadedStrings #-}
module TypesSpec where

import Test.Hspec
import Data.Aeson

import Types

spec :: Spec
spec = do
  describe "json import and export" $ do
    it "exports to json with kebab-case names" $ do
      encode (AccountInfo 1 "Mr. Pink" 200) `shouldBe`
        "{\"account-number\":1,\"name\":\"Mr. Pink\",\"balance\":200}"
    it "import after export is the identity" $ do
      let exampleAccount = AccountInfo 1 "Mr. Pink" 200
      decode (encode exampleAccount) `shouldBe` Just exampleAccount
