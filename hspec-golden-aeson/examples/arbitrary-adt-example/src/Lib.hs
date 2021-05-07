{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( Person (..),
    OnOrOff (..),
  )
where

-- base

-- Cereal
import Data.Serialize
import GHC.Generics (Generic)
-- QuickCheck
import Test.QuickCheck (Arbitrary (..), oneof)
-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

data Person = Person
  { name :: String,
    address :: String,
    age :: Int
  }
  deriving (Eq, Read, Show, Generic)

instance Serialize Person

instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

data OnOrOff
  = On
  | Off
  deriving (Eq, Read, Show, Generic)

instance Serialize OnOrOff

instance ToADTArbitrary OnOrOff

instance Arbitrary OnOrOff where
  arbitrary = oneof [pure On, pure Off]
