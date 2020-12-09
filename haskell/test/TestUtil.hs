module TestUtil where

import Data.Aeson.Types (Pair)
import Test.Hspec.Wai
import Data.Aeson

import Database

withTestDB = individualConnection testDB

jsonBody :: Value -> ResponseMatcher
jsonBody expected = ResponseMatcher 200 [] (MatchBody (\_ b -> result b))
  where
  result b = case (decode b) of
    Nothing -> Just $ "could not decode json body " <> (show b)
    Just actual -> if actual == expected
      then Nothing
      else Just $ actualExpected "json body does not match" (show b) (show $ encode $ expected)

actualExpected :: String -> String -> String -> String
actualExpected message actual expected = unlines [
    message
  , "  expected: " ++ expected
  , "  but got:  " ++ actual
  ]
