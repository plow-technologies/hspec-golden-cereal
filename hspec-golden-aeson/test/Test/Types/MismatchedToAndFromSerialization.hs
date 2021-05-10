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
data WeirdPerson = WeirdPerson
  {
    personAge :: Int,
    personName :: String
  }  
  deriving (Eq, Show, Generic)

instance Serialize WeirdPerson

instance Serialize Person where
  put = \(Person n a) -> put (WeirdPerson a n)


instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = genericArbitrary
