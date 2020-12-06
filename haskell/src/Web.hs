{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty

import Types

bank :: ScottyM ()
bank = do
  get "/account/:id" $ do
    json $ AccountInfo 1 "Mr. Orange" 500
  get "/s/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
