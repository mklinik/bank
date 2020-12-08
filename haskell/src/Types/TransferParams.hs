{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types.TransferParams where

import GHC.Generics
import Data.Aeson
import Text.Casing (kebab)

data TransferParams = TransferParams
  { amount :: Int
  , account_number :: Int
  } deriving (Generic, Show, Eq)

withKebabLabels = defaultOptions {fieldLabelModifier = kebab}

instance FromJSON TransferParams where
  parseJSON = genericParseJSON withKebabLabels
