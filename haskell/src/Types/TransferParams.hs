{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types.TransferParams where

import GHC.Generics
import Data.Aeson
import Text.Casing (kebab)

import Types (Amount)

data TransferParams = TransferParams
  { amount :: Amount
  , account_number :: Int
  } deriving (Generic, Show, Eq)

withKebabLabels = defaultOptions {fieldLabelModifier = kebab}

instance FromJSON TransferParams where
  parseJSON = genericParseJSON withKebabLabels
