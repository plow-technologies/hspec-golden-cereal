{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cereal.Internal.Utils
-- Description : Internal types, functions and values
-- Copyright   : (c) Plow Technologies, 2016
-- License     : BSD3
-- Maintainer  : mchaver@gmail.com
-- Stability   : Beta
module Test.Cereal.Internal.ADT.Utils where

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int32)
import Data.Proxy
import Data.Serialize
import Data.Typeable
import GHC.Generics
import Test.Cereal.Internal.Utils (RandomSamples (..))
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Prelude

-- | This function will compare one JSON encoding to a subsequent JSON encoding, thus eliminating the need for an Eq instance
checkAesonEncodingEquality :: forall a. Serialize a => a -> Bool
checkAesonEncodingEquality a =
  let byteStrA = encodeLazy a
      decodedVal = (decodeLazy byteStrA) :: Either String a
      eitherByteStrB = encodeLazy <$> decodedVal
   in (Right byteStrA) == eitherByteStrB

-- | run decode in IO, if it returns Left then throw an error.
aesonDecodeIO :: Serialize a => ByteString -> IO a
aesonDecodeIO bs = case decodeLazy bs of
  Right a -> return a
  Left msg -> throwIO $ AesonDecodeError msg

data AesonDecodeError = AesonDecodeError String
  deriving (Show, Eq)

instance Exception AesonDecodeError

instance Serialize a => Serialize (RandomSamples a)

encodePrettySortedKeys :: Serialize a => a -> ByteString
encodePrettySortedKeys = encodeLazy

-- | Reads the seed without looking at the samples.
readSeed :: ByteString -> IO Int32
readSeed = fmap seed . aesonDecodeIO @(RandomSamples ByteString)

-- | Read the sample size.
readSampleSize :: ByteString -> IO Int
readSampleSize = fmap (length . samples) . aesonDecodeIO @(RandomSamples ByteString)
