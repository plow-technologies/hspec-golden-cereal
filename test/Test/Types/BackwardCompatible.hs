{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Types.BackwardCompatible where

import Data.Serialize
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import qualified Test.Types as V0 (Person (Person))

data Person = Person
  { name :: String
  }
  deriving (Eq, Show, Generic)

instance Serialize Person where
  put x = do
    putWord16le 1
    put $ name x
  get = do
    v <- getWord16le
    case v of
      0 -> fmap fromOldFormat (V0.Person <$> get <*> get)
      1 -> Person <$> get
      _ -> fail $ "Unknown version: " ++ show v
    where
       fromOldFormat :: V0.Person -> Person
       fromOldFormat (V0.Person n _) = Person n

instance ToADTArbitrary Person

instance Arbitrary Person where
  arbitrary = genericArbitrary
