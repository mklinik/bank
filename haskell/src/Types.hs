{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics
import Data.Aeson
import Text.Casing (kebab)

data AccountInfo = AccountInfo
  { account_number :: Int
  , name :: String
  , balance :: Int
  } deriving (Generic, Show, Eq)

instance ToJSON AccountInfo where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = kebab})

instance FromJSON AccountInfo
