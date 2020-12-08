{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types.DepositParams where

import GHC.Generics
import Data.Aeson

data DepositParams = DepositParams
  { amount :: Int
  } deriving (Generic, Show, Eq)

instance FromJSON DepositParams
