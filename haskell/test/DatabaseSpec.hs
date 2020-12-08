{-# LANGUAGE OverloadedStrings #-}
module DatabaseSpec where

import Test.Hspec
import Database.PostgreSQL.Simple

import Database
import Types

spec :: Spec
spec = before_ (withDefaultConnection resetDatabase) $ do
  describe "resetDatabase" $ do
    it "cretes an empty accounts table" $ do
      result <- withDefaultConnection (\c -> query_ c "select * from account")
      result `shouldBe` ([] :: [(Int, String, Int)])
    it "cretes an empty accounts table, and parses to AccountInfo" $ do
      result <- withDefaultConnection (\c -> query_ c "select * from account")
      result `shouldBe` ([] :: [AccountInfo])

  describe "createAccount" $ do
    it "cretes an account and returns it" $ do
      withDefaultConnection (createAccount "Mr. Pink")
        `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]
    it "cretes multiple accounts with ascending account number" $ do
      withDefaultConnection (createAccount "Mr. Pink")
        `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]
      withDefaultConnection (createAccount "Mr. Orange")
        `shouldReturn` [AccountInfo 2 "Mr. Orange" 0]

  describe "getAccount" $ do
    it "retrieves an account" $ do
      withDefaultConnection (createAccount "Mr. Pink")
      withDefaultConnection (getAccount 1) `shouldReturn` [AccountInfo 1 "Mr. Pink" 0]

    it "retrieves multiple accounts" $ do
      sequence_ [withDefaultConnection (createAccount name)
        | name <- ["Mr. Orange", "Mr. Pink", "Mr. Black", "Mr. White"]]
      withDefaultConnection (getAccount 4) `shouldReturn` [AccountInfo 4 "Mr. White" 0]
      withDefaultConnection (getAccount 3) `shouldReturn` [AccountInfo 3 "Mr. Black" 0]
      withDefaultConnection (getAccount 2) `shouldReturn` [AccountInfo 2 "Mr. Pink" 0]
      withDefaultConnection (getAccount 1) `shouldReturn` [AccountInfo 1 "Mr. Orange" 0]

    context "when trying to retrieve a non-existing account" $
      it "returns the empty list" $ do
        withDefaultConnection (createAccount "Mr. Pink")
        withDefaultConnection (getAccount 42) `shouldReturn` []
