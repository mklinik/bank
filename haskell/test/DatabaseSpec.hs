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
        `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
    it "cretes multiple accounts with ascending account number" $ do
      withTestDB (createAccount "Mr. Pink")
        `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
      withTestDB (createAccount "Mr. Orange")
        `shouldReturn` Success (AccountInfo 2 "Mr. Orange" 0)

  describe "getAccount" $ do
    it "retrieves an account" $ do
      withTestDB (createAccount "Mr. Pink")
      withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)

    it "retrieves multiple accounts" $ do
      sequence_ [withTestDB (createAccount name)
        | name <- ["Mr. Orange", "Mr. Pink", "Mr. Black", "Mr. White"]]
      withTestDB (getAccount 4) `shouldReturn` Success (AccountInfo 4 "Mr. White" 0)
      withTestDB (getAccount 3) `shouldReturn` Success (AccountInfo 3 "Mr. Black" 0)
      withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. Pink" 0)
      withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Orange" 0)

    context "when trying to retrieve a non-existing account" $
      it "returns the empty list" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (getAccount 42) `shouldReturn` ENoSuchAccount

  describe "depositMoney" $ do
    it "deposits money in the happy case" $ do
      withTestDB (createAccount "Mr. Pink")
      withTestDB (depositMoney 1 50) `shouldReturn` [AccountInfo 1 "Mr. Pink" 50]

    context "when there are multiple accounts" $ do
      it "deposits money into the requested account" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (createAccount "Mr. Orange")
        withTestDB (depositMoney 2 50) `shouldReturn` [AccountInfo 2 "Mr. White" 50]
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 50)
        withTestDB (getAccount 3) `shouldReturn` Success (AccountInfo 3 "Mr. Orange" 0)

    context "when a non-existing account number is given" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (createAccount "Mr. Orange")
        withTestDB (depositMoney 400 50) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 0)
        withTestDB (getAccount 3) `shouldReturn` Success (AccountInfo 3 "Mr. Orange" 0)

    context "when a negative amount is given" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (createAccount "Mr. Orange")
        withTestDB (depositMoney 2 (-50)) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 0)
        withTestDB (getAccount 3) `shouldReturn` Success (AccountInfo 3 "Mr. Orange" 0)


  describe "withdrawMoney" $ do
    it "withdraws money in the happy case" $ do
      withTestDB (createAccount "Mr. Pink")
      withTestDB (depositMoney 1 50)
      withTestDB (withdrawMoney 1 20) `shouldReturn` [AccountInfo 1 "Mr. Pink" 30]

    context "when there are multiple accounts" $ do
      it "withdraws only from the given account" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (createAccount "Mr. Orange")
        withTestDB (depositMoney 2 50)
        withTestDB (withdrawMoney 2 11) `shouldReturn` [AccountInfo 2 "Mr. White" 39]
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 39)
        withTestDB (getAccount 3) `shouldReturn` Success (AccountInfo 3 "Mr. Orange" 0)

    context "when a negative amount is given" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (createAccount "Mr. Orange")
        withTestDB (depositMoney 2 50)
        withTestDB (withdrawMoney 2 (-11)) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 50)
        withTestDB (getAccount 3) `shouldReturn` Success (AccountInfo 3 "Mr. Orange" 0)

    context "when a non-existing account number is given" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (createAccount "Mr. Orange")
        withTestDB (withdrawMoney (-50) 11) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 0)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 0)
        withTestDB (getAccount 3) `shouldReturn` Success (AccountInfo 3 "Mr. Orange" 0)

    context "when trying to withdraw more than available" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (depositMoney 1 50)
        withTestDB (withdrawMoney 1 1000) `shouldReturn` []


  describe "transferMoney" $ do
    it "transfers money from one account to another in the happy case" $ do
      withTestDB (createAccount "Mr. Pink")
      withTestDB (createAccount "Mr. White")
      withTestDB (depositMoney 1 50)
      withTestDB (transferMoney 1 2 49) `shouldReturn` [AccountInfo 1 "Mr. Pink" 1]
      withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 1)
      withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 49)

    context "when sender is equal to receiver" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (depositMoney 1 50)
        withTestDB (transferMoney 1 1 49) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 50)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 0)

    context "when amount is negative" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (depositMoney 1 50)
        withTestDB (transferMoney 1 2 (-5)) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 50)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 0)

    context "when sender has insufficient funds" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (depositMoney 1 50)
        withTestDB (transferMoney 1 2 5000) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 50)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 0)

    context "when sender does not exist" $ do
      it "does nothing" $ do
        withTestDB (createAccount "Mr. Pink")
        withTestDB (createAccount "Mr. White")
        withTestDB (depositMoney 1 50)
        withTestDB (transferMoney 42 2 25) `shouldReturn` []
        withTestDB (getAccount 1) `shouldReturn` Success (AccountInfo 1 "Mr. Pink" 50)
        withTestDB (getAccount 2) `shouldReturn` Success (AccountInfo 2 "Mr. White" 0)
