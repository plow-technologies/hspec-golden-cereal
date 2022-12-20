{-# LANGUAGE DeriveGeneric #-}

module Test.Types.AlteredSelector where

import Data.Serialize
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Person = Person
  { name :: String,
    height :: Float
  }
  deriving (Eq, Show, Generic)

instance Serialize Person

instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = genericArbitrary
