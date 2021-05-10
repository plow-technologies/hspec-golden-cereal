{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}


-- |
-- Module      : Test.Cereal.GenericSpecs
-- Description : Export all necessary functions
-- Copyright   : (c) Plow Technologies, 2016
-- License     : BSD3
-- Maintainer  : mchaver@gmail.com
-- Stability   : Beta
--
-- This package provides tools for testing Aeson serialization.
--
-- - Test that 'ToJSON' and 'FromJSON' instances are isomorphic.
-- - Alert you when unexpected changes in Aeson serialization occur.
-- - Record JSON formatting of Haskell types.
module Test.Cereal.GenericSpecs 
  (
    -- * Arbitrary testing
    goldenSpecs
  , roundtripSpecs
  , roundtripAndGoldenSpecs

  -- * ToADTArbitrary testing
  , goldenADTSpecs
  , roundtripADTSpecs
  , roundtripAndGoldenSpecsWithSettings
  , roundtripAndGoldenADTSpecs
  , roundtripAndGoldenADTSpecsWithSettings

  -- * Make Files
  , mkGoldenFileForType

  -- * Util
  , shouldBeIdentity
  , GoldenDirectoryOption(..)
  , Settings(..)
  , defaultSettings

  -- * re-exports
  , Proxy(..)
  ) where
    
import Data.Proxy
import qualified Data.Serialize as Cereal
import Data.Typeable
import Test.Cereal.Internal.ADT.GoldenSpecs (goldenADTSpecs, mkGoldenFileForType)
import Test.Cereal.Internal.ADT.RoundtripSpecs (roundtripADTSpecs)
import Test.Cereal.Internal.ADT.Utils()
import qualified Test.Cereal.Internal.GoldenSpecs as Golden
import qualified Test.Cereal.Internal.RoundtripSpecs as Roundtrip
import Test.Cereal.Internal.Utils
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import GHC.Generics

roundtripSpecs :: forall a . (Arbitrary a, Cereal.Serialize a, Typeable a, Show a) => Proxy a -> Spec
roundtripSpecs Proxy = Roundtrip.roundtripSpecs (Proxy :: Proxy (GoldenCereal a))

goldenSpecs :: 
  forall a.
  (Arbitrary a, Cereal.Serialize a, Typeable a, Show a) =>
  Settings ->
  Proxy a ->
  Spec
goldenSpecs settings Proxy = Golden.goldenSpecs settings (Proxy :: Proxy (GoldenCereal a))

-- | run roundtrip and golden test for a type.
-- sampleSize is used only when creating the golden file. When it is
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenSpecs :: (Arbitrary a, Cereal.Serialize a, Typeable a, Show a) => Proxy a -> Spec
roundtripAndGoldenSpecs proxy =
  roundtripAndGoldenSpecsWithSettings defaultSettings proxy

-- | 'roundtripAndGoldenSpecs' with custom settings.
roundtripAndGoldenSpecsWithSettings ::
  forall a.
  (Arbitrary a, Cereal.Serialize a, Typeable a, Show a) =>
  Settings ->
  Proxy a ->
  Spec
roundtripAndGoldenSpecsWithSettings settings proxy = do
  roundtripSpecs proxy
  goldenSpecs settings proxy

-- | run roundtrip and golden tests for all constructors of a type.
-- sampleSize is used only when creating the golden files. When they are
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenADTSpecs ::
  forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, Cereal.Serialize a) =>
  Proxy a ->
  Spec
roundtripAndGoldenADTSpecs proxy =
  roundtripAndGoldenADTSpecsWithSettings defaultSettings proxy

-- | 'roundtripAndGoldenADTSpecs' with custom settings.
roundtripAndGoldenADTSpecsWithSettings ::
  forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, Cereal.Serialize a) =>
  Settings ->
  Proxy a ->
  Spec
roundtripAndGoldenADTSpecsWithSettings settings proxy = do
  roundtripADTSpecs proxy
  goldenADTSpecs settings proxy

data GoldenCereal a = GoldenCereal a deriving (Show)

instance GoldenSerializer GoldenCereal where
  type Ctx GoldenCereal = Cereal.Serialize
  encode = Cereal.encodeLazy . unlift
  decode = fmap lift . Cereal.decodeLazy
  lift = GoldenCereal
  unlift (GoldenCereal a) = a

instance Arbitrary a => Arbitrary (GoldenCereal a) where
  arbitrary = GoldenCereal <$> arbitrary


data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show, Generic)

instance Cereal.Serialize Person

instance Arbitrary Person where
  arbitrary = genericArbitrary