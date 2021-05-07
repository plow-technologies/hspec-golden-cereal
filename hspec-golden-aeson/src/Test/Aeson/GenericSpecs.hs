{-|
Module      : Test.Aeson.GenericSpecs
Description : Export all necessary functions
Copyright   : (c) Plow Technologies, 2016
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

This package provides tools for testing Aeson serialization.

- Test that 'ToJSON' and 'FromJSON' instances are isomorphic.
- Alert you when unexpected changes in Aeson serialization occur.
- Record JSON formatting of Haskell types.

-}

{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE KindSignatures #-}

module Test.Aeson.GenericSpecs
{-  (
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
  )-} where

import           Data.Aeson                            hiding (encode, decode)

import           Data.Proxy
import           Data.Typeable

import           Test.Aeson.Internal.ADT.GoldenSpecs    (goldenADTSpecs, mkGoldenFileForType)
import           Test.Aeson.Internal.ADT.RoundtripSpecs (roundtripADTSpecs)
import           Test.Aeson.Internal.GoldenSpecs        (goldenSpecs)
import           Test.Aeson.Internal.RoundtripSpecs     (roundtripSpecs)
import           Test.Aeson.Internal.Utils
import qualified           Test.Aeson.Internal.ADT.Utils as Temp
import           Test.Hspec
import           Test.QuickCheck
import Data.Aeson.Encode.Pretty
import           Test.QuickCheck.Arbitrary.ADT
import           Data.ByteString.Lazy (ByteString)
import GHC.Exts

-- | run roundtrip and golden test for a type.
-- sampleSize is used only when creating the golden file. When it is
-- compared, the sampleSize is derived from the file.
--roundtripAndGoldenSpecs :: forall a.
--  (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
--  => Proxy a -> Spec
roundtripAndGoldenSpecs proxy =
  roundtripAndGoldenSpecsWithSettings defaultSettings proxy

-- | 'roundtripAndGoldenSpecs' with custom settings.
--roundtripAndGoldenSpecsWithSettings :: forall a.
--  (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
--  => Settings -> Proxy a -> Spec
roundtripAndGoldenSpecsWithSettings settings proxy = do
  roundtripSpecs proxy
  --goldenSpecs @GoldenJson1 settings proxy

-- | run roundtrip and golden tests for all constructors of a type.
-- sampleSize is used only when creating the golden files. When they are
-- compared, the sampleSize is derived from the file.
roundtripAndGoldenADTSpecs :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Proxy a -> Spec
roundtripAndGoldenADTSpecs proxy =
  roundtripAndGoldenADTSpecsWithSettings Temp.defaultSettings proxy

-- | 'roundtripAndGoldenADTSpecs' with custom settings.
roundtripAndGoldenADTSpecsWithSettings :: forall a.
  (Arbitrary a, ToADTArbitrary a, Eq a, Show a, ToJSON a, FromJSON a)
  => Temp.Settings -> Proxy a -> Spec
roundtripAndGoldenADTSpecsWithSettings settings proxy = do
  roundtripADTSpecs proxy
  goldenADTSpecs settings proxy


encodePrettySortedKeys :: ToJSON a => a -> ByteString
encodePrettySortedKeys = encodePretty' defConfig { confCompare = compare }

instance FromJSON a => FromJSON (RandomSamples a)
instance ToJSON   a => ToJSON   (RandomSamples a)

data GoldenJson a = GoldenJson a deriving (Show)

instance GoldenSerializer1 GoldenJson where

  decode _ = Left "hello"
  lift a = GoldenJson a
  unlift (GoldenJson a) = a

class GoldenSerializer1 (s :: * -> *) where
  decode :: String -> Either String (s a)
  lift :: a -> s a
  unlift :: s a -> a


encode12 :: (ToJSON a) => GoldenJson a -> ByteString
encode12 (GoldenJson a) = encodePrettySortedKeys a


test :: (s a -> ByteString) -> s a -> ByteString
test f s = f s

test2 = test encode12 (GoldenJson ())