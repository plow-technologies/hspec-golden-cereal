{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import Data.Serialize
import Test.Cereal.Internal.Utils (RandomSamples (..))
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