{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Types.BrokenSerialization where

import Data.Serialize
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show, Generic)

data PersonWithoutAge = PersonWithoutAge
  {
    nameWithoutAge :: String
  }
  deriving (Eq, Show, Generic)

instance Serialize PersonWithoutAge

instance Serialize Person where
  put = \(Person newName _) -> put (PersonWithoutAge newName)


instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = genericArbitrary

data SumType
  = SumType1 Int
  | SumType2 String Int
  | SumType3 Double String Int
  deriving (Eq, Show, Generic)

instance Serialize SumType

instance ToADTArbitrary SumType

instance Arbitrary SumType where
  arbitrary = genericArbitrary

data P2 = P2 String Int deriving (Eq, Show, Generic)
instance Serialize P2