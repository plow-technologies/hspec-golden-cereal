{-|
Module      : Test.Cereal.Internal.RoundtripFromFile
Description : Golden tests for Arbitrary
Copyright   : (c) Plow Technologies, 2022
License     : BSD3
Maintainer  : bruno.cadorette@plowtech.net
Stability   : Beta

Internal module, use at your own risk.
-}

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Test.Cereal.Internal.ADT.RoundtripFromFile where

import           Control.Exception
import           Control.Monad

import qualified Data.Serialize as Cereal
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Typeable

import           Prelude hiding (readFile, writeFile)

import           System.Directory
import           System.Environment (lookupEnv)

import           Test.Cereal.Internal.Utils
import           Test.Hspec
import           Test.HUnit.Lang (HUnitFailure)
import           Test.Cereal.Internal.ADT.GoldenSpecs
import           Test.QuickCheck.Arbitrary.ADT
import qualified Test.Cereal.Internal.RoundtripFromFile



roundtripADTFromFile :: forall a. 
  (Typeable a, ToADTArbitrary a, Eq a, Show a, Cereal.Serialize a)
  => Settings -> Proxy a -> Spec
roundtripADTFromFile settings proxy = do
  (moduleName,typeName,constructors) <- runIO $ generateInfoFromADT proxy
  describe ("Cereal encoding of " ++ typeName) $
    mapM_ (testConstructorRoundtrip settings moduleName typeName) constructors

testConstructorRoundtrip :: forall a. (Eq a, Show a, Cereal.Serialize a, ToADTArbitrary a) =>
  Settings -> String -> String -> ConstructorArbitraryPair a -> SpecWith ( Arg (IO ()))
testConstructorRoundtrip Settings{..} moduleName typeName cap = do
  it ("produces the same encoding as is found in " ++ goldenFile) $ do
    createMissing <- isJust <$> lookupEnv "CREATE_MISSING_GOLDEN"
    fileExist <- doesFileExist goldenFile
    if fileExist then
      Test.Cereal.Internal.RoundtripFromFile.runTestFromFile (Proxy @a) goldenFile 
        `catches` 
          [ 
            Handler (\(err :: HUnitFailure) -> fixIfFlag err), 
            Handler (\(err :: DecodeError) -> fixIfFlag err)
          ]
    else if createMissing then
      createGoldenFile sampleSize cap goldenFile
    else
      expectationFailure $ "Missing golden file: " ++ goldenFile

  where
    goldenFile = mkGoldenFilePath topDir mModuleName typeName cap
    topDir = case goldenDirectoryOption of
      GoldenDirectory -> "golden"
      CustomDirectoryName d -> d
    mModuleName = case useModuleNameAsSubDirectory of
      True  -> Just moduleName
      False -> Nothing
    
    fixIfFlag err = do
      doFix <- isJust <$> lookupEnv "RECREATE_BROKEN_GOLDEN"
      if doFix then do
        createGoldenFile sampleSize cap goldenFile
      else 
        throwIO err