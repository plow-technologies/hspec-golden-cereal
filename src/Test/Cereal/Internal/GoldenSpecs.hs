{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Test.Cereal.Internal.GoldenSpecs
-- Description : Golden tests for Arbitrary
-- Copyright   : (c) Plow Technologies, 2021
-- License     : BSD3
-- Maintainer  : bruno-cadorette@plowtech.net
-- Stability   : Beta
--
-- Internal module, use at your own risk.
module Test.Cereal.Internal.GoldenSpecs where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy hiding (putStrLn)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Proxy
import qualified Data.Serialize as Cereal (Serialize, decodeLazy, encodeLazy)
import Data.Typeable
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.Random
import Test.Cereal.Internal.Utils
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (readFile, writeFile)

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
-- COMPATIBILITY_CHECK mode: 
--  By using this mode checks with golden files are in terms of type compatibility instead of byte for byte.
--  This is useful for checking forward or backward compatibility of types that evolves in time.
goldenSpecs ::
  forall s a.
  (GoldenSerializerConstraints s a, Typeable a, Arbitrary a, Cereal.Serialize a) =>
  Settings ->
  Proxy (s a) ->
  Spec
goldenSpecs settings proxy =
  goldenSpecsWithNote settings proxy Nothing

-- | same as 'goldenSpecs' but has the option of passing a note to the
-- 'describe' function.
goldenSpecsWithNote ::
  forall s a.
  (GoldenSerializerConstraints s a, Typeable a, Arbitrary a, Cereal.Serialize a) =>
  Settings ->
  Proxy (s a) ->
  Maybe String ->
  Spec
goldenSpecsWithNote settings proxy mNote = do
  typeNameInfo <- runIO $ mkTypeNameInfo settings (Proxy :: Proxy a)
  goldenSpecsWithNotePlain settings typeNameInfo proxy mNote

-- | same as 'goldenSpecsWithNote' but does not require a Typeable, Eq or Show instance.
goldenSpecsWithNotePlain ::
  forall s a.
  (GoldenSerializerConstraints s a, Arbitrary a, Cereal.Serialize a) =>
  Settings ->
  TypeNameInfo a ->
  Proxy (s a) ->
  Maybe String ->
  Spec
goldenSpecsWithNotePlain settings@Settings {..} typeNameInfo@(TypeNameInfo {typeNameTypeName}) proxy mNote = do
  let goldenFile = mkGoldenFile settings typeNameInfo
      note = maybe "" (" " ++) mNote
  describe ("Encoding of " ++ addBrackets (unTypeName typeNameTypeName) ++ note) $
    it ("produces the same data as is found in " ++ goldenFile) $ do
      exists <- doesFileExist goldenFile
      let fixIfFlag err = do
            doFix <- isJust <$> lookupEnv recreateBrokenGoldenEnv
            if doFix
              then createGoldenfile @s settings proxy goldenFile
              else throwIO err
      if exists
        then do
          doCompatibility <- isJust <$> lookupEnv compatibilityCheckEnv
          if doCompatibility
            then do
              putStrLn "running golden tests in compatibility mode"
              compareCompatibilityWithGolden @s settings proxy goldenFile comparisonFile
            else do
              putStrLn "running golden tests using byte for byte check"
              compareWithGolden @s settings typeNameInfo proxy goldenFile comparisonFile
                `catches` [ Handler (\(err :: HUnitFailure) -> fixIfFlag err),
                            Handler (\(err :: DecodeError) -> fixIfFlag err)
                          ]
        else do
          doCreate <- isJust <$> lookupEnv createMissingGoldenEnv
          if doCreate
            then do 
              putStrLn "creating golden tests"
              createGoldenfile @s settings proxy goldenFile
            else expectationFailure $ "Missing golden file: " <> goldenFile

-- | PRE-condition: Golden file already exist.
--   Try to decode golden file and encode it with the current encoder,
--   then compare both encoded representations (byte for byte check).
compareWithGolden ::
  forall s a.
  (GoldenSerializerConstraints s a, Arbitrary a, Cereal.Serialize a) =>
  Settings ->
  TypeNameInfo a ->
  Proxy (s a) ->
  FilePath ->
  ComparisonFile ->
  IO ()
compareWithGolden _settings _typeNameInfo _Proxy goldenFile _comparisonFile = do
  bytes <- readFile goldenFile
  case Cereal.decodeLazy bytes of
    Right (randomSamples :: RandomSamples a) ->
      Cereal.encodeLazy randomSamples `shouldBe` bytes
    Left decodingError -> expectationFailure $ "Failed to decode the encoded values of the golden file: " ++ decodingError

-- | PRE-condition: Golden file already exist.
--   Try to decode the golden file, then re-encode and re-decode it again,
--   finally compare initially decoded values with latest decoded ones (at type compatibility level)
compareCompatibilityWithGolden ::
  forall s a.
  (GoldenSerializerConstraints s a, Arbitrary a, Eq a, Cereal.Serialize a) =>
  Settings ->
  Proxy (s a) ->
  FilePath ->
  ComparisonFile ->
  IO ()
compareCompatibilityWithGolden _settings _Proxy goldenFile _comparisonFile = do
  goldenBytes <- readFile goldenFile
  case Cereal.decodeLazy goldenBytes of
    Left decodingError -> expectationFailure $ "Failed to decode the encoded values of the golden file: " ++ decodingError
    Right (randomSamples :: RandomSamples a) -> do
      let reEncodedGoldenBytes = Cereal.encodeLazy randomSamples 
      case Cereal.decodeLazy reEncodedGoldenBytes of
        Left decodingError -> expectationFailure $ "Failed to re-decode the re-encoded values of the golden file: " ++ decodingError
        Right reDecodedRandomSamples ->
         randomSamples `shouldBe` reDecodedRandomSamples

-- | The golden files do not exist. Create it.
createGoldenfile :: forall s a. (Ctx s (RandomSamples a), GoldenSerializer s, Arbitrary a) => Settings -> Proxy (s a) -> FilePath -> IO ()
createGoldenfile Settings {..} Proxy goldenFile = do
  createDirectoryIfMissing True (takeDirectory goldenFile)
  rSeed <- randomIO
  rSamples <- lift @s <$> mkRandomSamples sampleSize (Proxy :: Proxy a) rSeed
  writeFile goldenFile (encode rSamples)

  putStrLn $
    "\n"
      ++ "WARNING: Running for the first time, not testing anything.\n"
      ++ "  Created "
      ++ goldenFile
      ++ " containing random samples,\n"
      ++ "  will compare "
      ++ fileType
      ++ " encodings with this from now on.\n"
      ++ "  Please, consider putting "
      ++ goldenFile
      ++ " under version control."

-- | Create the file path for the golden file. Optionally use the module name to
-- help avoid name collissions. Different modules can have types of the same
-- name.
mkGoldenFile :: Settings -> TypeNameInfo a -> FilePath
mkGoldenFile Settings {..} (TypeNameInfo {typeNameTopDir, typeNameModuleName, typeNameTypeName}) =
  case typeNameModuleName of
    Nothing -> unTopDir typeNameTopDir </> unTypeName typeNameTypeName <.> fileType
    Just moduleName -> unTopDir typeNameTopDir </> unModuleName moduleName </> unTypeName typeNameTypeName <.> fileType

-- | Create a number of arbitrary instances of a type
-- a sample size and a random seed.
mkRandomSamples ::
  forall a.
  (Arbitrary a) =>
  Int ->
  Proxy a ->
  Int32 ->
  IO (RandomSamples a)
mkRandomSamples sampleSize Proxy rSeed = RandomSamples rSeed <$> generate gen
  where
    correctedSampleSize = if sampleSize <= 0 then 1 else sampleSize
    gen :: Gen [a]
    gen = setSeed (fromIntegral rSeed) $ replicateM correctedSampleSize (arbitrary :: Gen a)
