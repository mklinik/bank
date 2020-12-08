module WaiUtil where

import Data.Aeson.Types (Pair)
import Test.Hspec.Wai
import Data.Aeson

jsonBody :: [Pair] -> ResponseMatcher
jsonBody expected = ResponseMatcher 200 [] (MatchBody (\_ b -> result b))
  where
  result b = case (decode b) of
    Nothing -> Just $ "could not decode json body " <> (show b)
    Just actual -> if actual == object expected
      then Nothing
      else Just $ actualExpected "json body does not match" (show b) (show $ encode $ object expected)

actualExpected :: String -> String -> String -> String
actualExpected message actual expected = unlines [
    message
  , "  expected: " ++ expected
  , "  but got:  " ++ actual
  ]
