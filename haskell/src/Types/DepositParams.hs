{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types.DepositParams where

import GHC.Generics
import Data.Aeson
import Types (Amount)

data DepositParams = DepositParams
  { amount :: Amount
  } deriving (Generic, Show, Eq)

instance FromJSON DepositParams
