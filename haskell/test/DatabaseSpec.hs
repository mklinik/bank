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

  describe "getAccount" $ do
    it "retrieves an account" $ do
      withConnection myConnectInfo reset
      withConnection myConnectInfo (createAccount "Mr. Pink")
      withConnection myConnectInfo (getAccount 1)
        `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]

    it "retrieves multiple accounts" $ do
      withConnection myConnectInfo reset
      sequence_ [withConnection myConnectInfo (createAccount name)
        | name <- ["Mr. Orange", "Mr. Pink", "Mr. Black", "Mr. White"]]
      withConnection myConnectInfo (getAccount 4)
        `shouldReturn` [AccountInfo 4 "Mr. White" 0]
      withConnection myConnectInfo (getAccount 3)
        `shouldReturn` [AccountInfo 3 "Mr. Black" 0]
      withConnection myConnectInfo (getAccount 2)
        `shouldReturn` [AccountInfo 2 "Mr. Pink" 0]
      withConnection myConnectInfo (getAccount 1)
        `shouldReturn` [AccountInfo 1 "Mr. Orange" 0]
