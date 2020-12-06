module Main where

import Web.Scotty

import Web

main :: IO ()
main = scotty 3000 bank
