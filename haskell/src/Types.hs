{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types where

import GHC.Generics
import Data.Aeson
import Text.Casing (kebab)
import Database.PostgreSQL.Simple (FromRow)

data AccountInfo = AccountInfo
  { account_number :: Int
  , name :: String
  , balance :: Int
  } deriving (Generic, Show, Eq, FromRow)

withKebabLabels = defaultOptions {fieldLabelModifier = kebab}

instance ToJSON AccountInfo where
  toEncoding = genericToEncoding withKebabLabels

instance FromJSON AccountInfo where
  parseJSON = genericParseJSON withKebabLabels
