{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types
  ( Amount
  , makeAmount
  , unsafeMakeAmount
  , getAmount
  , AccountInfo (..)
  ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Text.Casing (kebab)
import Database.PostgreSQL.Simple (FromRow)

newtype Amount = Amount { getAmount :: Int }
  deriving (Show, Eq, Generic)

makeAmount :: Int -> Maybe Amount
makeAmount amount
  | amount < 0 = Nothing
  | otherwise = Just $ Amount amount

-- For testing. Only pass positive integers here!
unsafeMakeAmount :: Int -> Amount
unsafeMakeAmount = Amount

instance FromJSON Amount where
  parseJSON v = do
    result <- parseJSON v
    case makeAmount result of
      Nothing -> fail "parsing amount failed"
      (Just amount) -> return amount

-- TODO: don't use the generic FromRow instance, write instance by hand.
data AccountInfo = AccountInfo
  { accountNumber :: Int
  , name :: String
  , balance :: Int
  } deriving (Generic, Show, Eq, FromRow)

withKebabLabels = defaultOptions {fieldLabelModifier = kebab}

instance ToJSON AccountInfo where
  toEncoding = genericToEncoding withKebabLabels

instance FromJSON AccountInfo where
  parseJSON = genericParseJSON withKebabLabels
