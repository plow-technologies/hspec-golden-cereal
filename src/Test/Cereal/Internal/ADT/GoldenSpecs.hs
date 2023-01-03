{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Test.Cereal.Internal.ADT.GoldenSpecs
-- Description : Golden tests for ToADTArbitrary
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno-cadorette@plowtech.net
-- Stability   : Beta
--
-- Internal module, use at your own risk.
module Test.Cereal.Internal.ADT.GoldenSpecs where

import Control.Arrow
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Proxy
import Data.Serialize
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.Random
import Test.Cereal.Internal.Utils
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Prelude hiding (readFile, writeFile)

-- | Tests to ensure that binary encoding has not unintentionally changed. This
-- could be caused by the following:
--
-- - A type's instances of the serialisation have changed.
-- - Selectors have been edited, added or deleted.
-- - You have changed version of Cereal the way Cereal serialization has changed
--   works.
--
-- If you run this function and the golden files do not
-- exist, it will create them for each constructor. It they do exist, it will
-- compare with golden file if it exists. Golden file encodes binary format of a
-- type. It is recommended that you put the golden files under revision control
-- to help monitor changes.
-- COMPATIBILITY_CHECK mode: 
--  By using this mode checks with golden files are in terms of type compatibility instead of byte for byte.
--  This is useful for checking forward or backward compatibility of types that evolves in time.
goldenADTSpecs ::
  forall a.
  (ToADTArbitrary a, Eq a, Show a, Serialize a) =>
  Settings ->
  Proxy a ->
  Spec
goldenADTSpecs settings proxy = goldenADTSpecsWithNote settings proxy Nothing

-- | same as 'goldenADTSpecs' but has the option of passing a note to the
-- 'describe' function.
goldenADTSpecsWithNote ::
  forall a.
  (ToADTArbitrary a, Eq a, Show a, Serialize a) =>
  Settings ->
  Proxy a ->
  Maybe String ->
  Spec
goldenADTSpecsWithNote settings proxy mNote = do
  (moduleName, typeName, constructors) <- runIO $ generateInfoFromADT proxy
  describe ("Binary encoding of " ++ typeName ++ note) $
    mapM_ (testConstructor settings moduleName typeName) constructors
  where
    note = maybe "" (" " ++) mNote

generateInfoFromADT :: ToADTArbitrary a => Proxy a -> IO (String, String, [ConstructorArbitraryPair a])
generateInfoFromADT proxy = fmap (\x -> (adtModuleName x, adtTypeName x, adtCAPs x)) <$> generate $ toADTArbitrary proxy

-- | test a single set of values from a constructor for a given type.
testConstructor ::
  forall a.
  (Eq a, Show a, Serialize a, ToADTArbitrary a) =>
  Settings ->
  String ->
  String ->
  ConstructorArbitraryPair a ->
  SpecWith (Arg (IO ()))
testConstructor Settings {..} moduleName typeName cap =
  it ("produces the same binary as is found in " ++ goldenFile) $ do
    exists <- doesFileExist goldenFile
    let fixIfFlag err = do
          doFix <- isJust <$> lookupEnv recreateBrokenGoldenEnv
          if doFix
            then createGoldenFile sampleSize cap goldenFile
            else throwIO err
    if exists
      then do
        doCompatibility <- isJust <$> lookupEnv compatibilityCheckEnv
        if doCompatibility
          then compareCompatibilityWithGolden topDir mModuleName typeName cap goldenFile
          else
            compareWithGolden topDir mModuleName typeName cap goldenFile
              `catches` [ Handler (\(err :: HUnitFailure) -> fixIfFlag err),
                          Handler (\(err :: DecodeError) -> fixIfFlag err)
                        ]
      else do
        doCreate <- isJust <$> lookupEnv createMissingGoldenEnv
        if doCreate
          then createGoldenFile sampleSize cap goldenFile
          else expectationFailure $ "Missing golden file: " <> goldenFile
  where
    goldenFile = mkGoldenFilePath topDir mModuleName typeName cap
    topDir = case goldenDirectoryOption of
      GoldenDirectory -> "golden"
      CustomDirectoryName d -> d
    mModuleName =
      if useModuleNameAsSubDirectory
        then Just moduleName
        else Nothing

-- | PRE-condition: Golden file already exist.
--   Try to decode golden file and encode it with the current encoder,
--   then compare both encoded representations (byte for byte check).
compareWithGolden ::
  forall a.
  (Show a, Eq a, Serialize a, ToADTArbitrary a) =>
  String ->
  Maybe String ->
  String ->
  ConstructorArbitraryPair a ->
  FilePath ->
  IO ()
compareWithGolden _topDir _mModuleName _typeName _cap goldenFile = do
  bytes <- LBS.readFile goldenFile
  case decodeLazy bytes of
    Right (randomSamples :: RandomSamples a) ->
      encodeLazy randomSamples `shouldBe` bytes
    Left err -> expectationFailure err

-- | PRE-condition: Golden file already exist.
--   Try to decode the golden file, then re-encode and re-decode it again,
--   finally compare initially decoded values with latest decoded ones (at type compatibility level)
compareCompatibilityWithGolden ::
  forall a.
  (Show a, Eq a, Serialize a, ToADTArbitrary a) =>
  String ->
  Maybe String ->
  String ->
  ConstructorArbitraryPair a ->
  FilePath ->
  IO ()
compareCompatibilityWithGolden _topDir _mModuleName _typeName _cap goldenFile = do
  goldenBytes <- LBS.readFile goldenFile
  case decodeLazy goldenBytes of
    Left decodingError -> expectationFailure $ "Failed to decode the encoded values of the golden file: " ++ decodingError
    Right (randomSamples :: RandomSamples a) -> do
      let reEncodedGoldenBytes = encodeLazy randomSamples 
      case decodeLazy reEncodedGoldenBytes of
        Left decodingError -> expectationFailure $ "Failed to re-decode the re-encoded values of the golden file: " ++ decodingError
        Right reDecodedRandomSamples ->
         randomSamples `shouldBe` reDecodedRandomSamples

-- | The golden files do not exist. Create them for each constructor.
createGoldenFile ::
  forall a.
  (Serialize a, ToADTArbitrary a) =>
  Int ->
  ConstructorArbitraryPair a ->
  FilePath ->
  IO ()
createGoldenFile sampleSize cap goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  rSeed <- randomIO :: IO Int32
  rSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor cap) rSeed
  LBS.writeFile goldenFile $ encodeLazy rSamples

  putStrLn $
    "\n"
      ++ "WARNING: Running for the first time, not testing anything.\n"
      ++ "  Created "
      ++ goldenFile
      ++ " containing random samples,\n"
      ++ "  will compare binary encodings with this from now on.\n"
      ++ "  Please, consider putting "
      ++ goldenFile
      ++ " under version control."

-- | Create the file path for the golden file. Optionally use the module name to
-- help avoid name collissions. Different modules can have types of the same
-- name.
mkGoldenFilePath :: forall a. FilePath -> Maybe FilePath -> FilePath -> ConstructorArbitraryPair a -> FilePath
mkGoldenFilePath topDir mModuleName typeName cap =
  case mModuleName of
    Nothing -> topDir </> typeName </> capConstructor cap <.> "bin"
    Just moduleName -> topDir </> moduleName </> typeName </> capConstructor cap <.> "bin"

-- | Create a number of arbitrary instances of a particular constructor given
-- a sample size and a random seed.
mkRandomADTSamplesForConstructor ::
  forall a.
  (ToADTArbitrary a) =>
  Int ->
  Proxy a ->
  String ->
  Int32 ->
  IO (RandomSamples a)
mkRandomADTSamplesForConstructor sampleSize Proxy conName rSeed = do
  generatedADTs <- generate gen
  let caps = concat $ adtCAPs <$> generatedADTs
      filteredCAPs = filter (\x -> capConstructor x == conName) caps
      arbs = capArbitrary <$> filteredCAPs
  return $ RandomSamples rSeed arbs
  where
    correctedSampleSize = if sampleSize <= 0 then 1 else sampleSize
    gen = setSeed (fromIntegral rSeed) $ replicateM correctedSampleSize (toADTArbitrary (Proxy :: Proxy a))

-- | Make a Golden File for the Proxy of a type if the file does not exist.
mkGoldenFileForType :: forall a. (Serialize a, ToADTArbitrary a) => Int -> Proxy a -> FilePath -> IO ()
mkGoldenFileForType sampleSize Proxy goldenPath = do
  (typeName, constructors) <- fmap (adtTypeName &&& adtCAPs) <$> generate $ toADTArbitrary (Proxy :: Proxy a)
  mapM_
    ( \constructor -> do
        let goldenFile = goldenPath </> typeName </> capConstructor constructor <.> ".bin"
        exists <- doesFileExist goldenFile
        if exists
          then pure ()
          else do
            createDirectoryIfMissing True (takeDirectory goldenFile)
            rSeed <- randomIO :: IO Int32
            rSamples <- mkRandomADTSamplesForConstructor sampleSize (Proxy :: Proxy a) (capConstructor constructor) rSeed
            LBS.writeFile goldenFile $ encodeLazy rSamples
    )
    constructors
