{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types.AccountParams where

import GHC.Generics
import Data.Aeson

data AccountParams = AccountParams
  { name :: String
  } deriving (Generic, Show, Eq)

instance FromJSON AccountParams
