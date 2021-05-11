{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- |
-- Module      : Test.Cereal.GenericSpecs
-- Description : Export all necessary functions
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno.cadorette@plowtech.net
-- Stability   : Beta
--
-- This package provides tools for testing Cereal serialization.
--
-- - Test that the Data.Serialize instance is isomorphic.
-- - Alert you when unexpected changes in Cereal serialization occur.
-- - Record binary formatting of Haskell types.
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
import qualified Test.Cereal.Internal.GoldenSpecs as Golden
import qualified Test.Cereal.Internal.RoundtripSpecs as Roundtrip
import Test.Cereal.Internal.Utils
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

roundtripSpecs :: forall a . (Arbitrary a, Cereal.Serialize a, Typeable a, Show a) => Proxy a -> Spec
roundtripSpecs Proxy = Roundtrip.roundtripSpecs (Proxy :: Proxy (GoldenCereal a))

-- | Tests to ensure that the binary encoding has not unintentionally changed. This
-- could be caused by the following:
--
-- - A type's instances of the serialisation have changed.
-- - Selectors have been edited, added or deleted.
-- - You have changed version of Cereal the way Cereal serialization has changed
--   works.
--
-- If you run this function and the golden files do not
-- exist, it will create them for each constructor. It they do exist, it will
-- compare with golden file if it exists. Golden file encodes the serialized format of a
-- type. It is recommended that you put the golden files under revision control
-- to help monitor changes.
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
roundtripAndGoldenADTSpecs =
  roundtripAndGoldenADTSpecsWithSettings defaultSettings

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

newtype GoldenCereal a = GoldenCereal a deriving (Show)

instance GoldenSerializer GoldenCereal where
  type Ctx GoldenCereal = Cereal.Serialize
  encode = Cereal.encodeLazy . unlift
  decode = fmap lift . Cereal.decodeLazy
  lift = GoldenCereal
  unlift (GoldenCereal a) = a

instance Arbitrary a => Arbitrary (GoldenCereal a) where
  arbitrary = GoldenCereal <$> arbitrary


defaultSettings :: Settings
defaultSettings = genericDefaultSettings "bin"