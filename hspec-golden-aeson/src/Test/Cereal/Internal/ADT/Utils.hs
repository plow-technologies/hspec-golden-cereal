{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module      : Test.Cereal.Internal.Utils
-- Description : Internal types, functions and values
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno-cadorette@plowtech.net
-- Stability   : Beta
module Test.Cereal.Internal.ADT.Utils where

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.Serialize
import Test.Cereal.Internal.Utils (RandomSamples (..), DecodeError(..))
import Prelude

-- | run decode in IO, if it returns Left then throw an error.
cerealDecodeIO :: Serialize a => ByteString -> IO a
cerealDecodeIO bs = case decodeLazy bs of
  Right a -> return a
  Left msg -> throwIO $ DecodeError msg

instance Serialize a => Serialize (RandomSamples a)