{-# LANGUAGE OverloadedStrings #-}
module DatabaseSpec where

import Test.Hspec
import Database.PostgreSQL.Simple

import TestUtil
import Database
import Types

spec :: Spec
spec = before_ (withTestDB resetDatabase) $ do
  describe "resetDatabase" $ do
    it "cretes an empty accounts table" $ do
      result <- withTestDB (\c -> query_ c "select * from account")
      result `shouldBe` ([] :: [(Int, String, Int)])
    it "cretes an empty accounts table, and parses to AccountInfo" $ do
      result <- withTestDB (\c -> query_ c "select * from account")
      result `shouldBe` ([] :: [AccountInfo])

  describe "createAccount" $ do
    it "cretes an account and returns it" $ do
      withTestDB (createAccount "Mr. Pink")
        `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]
    it "cretes multiple accounts with ascending account number" $ do
      withTestDB (createAccount "Mr. Pink")
        `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]
      withTestDB (createAccount "Mr. Orange")
        `shouldReturn` [AccountInfo 2 "Mr. Orange" 0]

  describe "getAccount" $ do
    it "retrieves an account" $ do
      withTestDB (createAccount "Mr. Pink")
      withTestDB (getAccount 1) `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]

    it "retrieves multiple accounts" $ do
      sequence_ [withTestDB (createAccount name)
        | name <- ["Mr. Orange", "Mr. Pink", "Mr. Black", "Mr. White"]]
      withTestDB (getAccount 4) `shouldReturn` [AccountInfo 4 "Mr. White" 0]
      withTestDB (getAccount 3) `shouldReturn` [AccountInfo 3 "Mr. Black" 0]
      withTestDB (getAccount 2) `shouldReturn` [AccountInfo 2 "Mr. Pink" 0]
      withTestDB (getAccount 1) `shouldReturn` [AccountInfo 1 "Mr. Orange" 0]

    context "when trying to retrieve a non-existing account" $
      it "returns the empty list" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (getAccount 42) `shouldReturn` []
