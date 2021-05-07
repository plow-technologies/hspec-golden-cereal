{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Types.MismatchedToAndFromSerialization where

import Data.Serialize
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show, Generic)

-- ToJSON and FromJSON use different strings, this should break.

instance Serialize Person where
  put = \(Person _ newAge) -> putInthost newAge
  get = pure (Person "name1" 1)

instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = genericArbitrary
