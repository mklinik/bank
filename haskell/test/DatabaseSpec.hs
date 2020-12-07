{-# LANGUAGE OverloadedStrings #-}
module DatabaseSpec where

import Test.Hspec
import Database.PostgreSQL.Simple

import Database
import Types

spec :: Spec
spec = do
  describe "reset" $ do
    it "cretes an empty accounts table" $ do
      withConnection myConnectInfo reset
      result <- withConnection myConnectInfo (\c -> query_ c "select * from account")
      result `shouldBe` ([] :: [(Int, String, Int)])
    it "cretes an empty accounts table, and parses to AccountInfo" $ do
      withConnection myConnectInfo reset
      result <- withConnection myConnectInfo (\c -> query_ c "select * from account")
      result `shouldBe` ([] :: [AccountInfo])

  describe "createAccount" $ do
    it "cretes an account and returns it" $ do
      withConnection myConnectInfo reset
      withConnection myConnectInfo (createAccount "Mr. Pink")
        `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]

    it "cretes multiple accounts with ascending account number" $ do
      withConnection myConnectInfo reset
      withConnection myConnectInfo (createAccount "Mr. Pink")
        `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]
      withConnection myConnectInfo (createAccount "Mr. Orange")
        `shouldReturn` [AccountInfo 2 "Mr. Orange" 0]
