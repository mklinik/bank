{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat)

import Lib
import Types

main :: IO ()
main = scotty 3000 $ do
  get "/account/:id" $ do
    json $ AccountInfo 1 "Mr. Orange" 500
  get "/s/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
