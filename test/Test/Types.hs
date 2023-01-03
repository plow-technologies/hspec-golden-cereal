{-# LANGUAGE DeriveGeneric #-}

module Test.Types where

import Data.Serialize
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show, Generic)

instance Serialize Person where
  put x = do
    putWord16le 0
    put $ name x
    put $ age x
  get = do
    v <- getWord16le
    case v of
      0 -> Person <$> get <*> get
      _ -> fail $ "Unknown version: " ++ show v

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
