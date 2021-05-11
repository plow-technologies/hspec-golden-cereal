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
-- Copyright   : (c) Plow Technologies, 2016
-- License     : BSD3
-- Maintainer  : mchaver@gmail.com
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

-- | Tests to ensure that the encoding has not unintentionally changed. This
-- could be caused by the following:
--
-- - A type's instances of the serialisation have changed.
-- - Selectors have been edited, added or deleted.
-- - You have changed version of Aeson the way Aeson serialization has changed
--   works.
--
-- If you run this function and the golden files do not
-- exist, it will create them for each constructor. It they do exist, it will
-- compare with golden file if it exists. Golden file encodes the serialized format of a
-- type. It is recommended that you put the golden files under revision control
-- to help monitor changes.
goldenSpecs ::
  forall s a.
  (GoldenSerializerConstraints s a, Typeable a, Arbitrary a) =>
  Settings ->
  Proxy (s a) ->
  Spec
goldenSpecs settings proxy = do
  runIO $ putStrLn "goldenSpecs"
  goldenSpecsWithNote settings proxy Nothing

-- | same as 'goldenSpecs' but has the option of passing a note to the
-- 'describe' function.
goldenSpecsWithNote ::
  forall s a.
  (GoldenSerializerConstraints s a, Typeable a, Arbitrary a) =>
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
  (GoldenSerializerConstraints s a, Arbitrary a) =>
  Settings ->
  TypeNameInfo a ->
  Proxy (s a) ->
  Maybe String ->
  Spec
goldenSpecsWithNotePlain settings@Settings {..} typeNameInfo@(TypeNameInfo {typeNameTypeName}) proxy mNote = do
  let goldenFile = mkGoldenFile settings typeNameInfo
      note = maybe "" (" " ++) mNote
  runIO $ putStrLn "goldenSpecsWithNotePlain"
  describe ("Encoding of " ++ addBrackets (unTypeName typeNameTypeName) ++ note) $
    it ("produces the same data as is found in " ++ goldenFile) $ do
      exists <- doesFileExist goldenFile
      putStrLn "does the file exist"
      let fixIfFlag err = do
            doFix <- isJust <$> lookupEnv recreateBrokenGoldenEnv
            if doFix
              then createGoldenfile @s settings proxy goldenFile
              else throwIO err
      if exists
        then
          compareWithGolden @s settings typeNameInfo proxy goldenFile comparisonFile
            `catches` [ Handler (\(err :: HUnitFailure) -> fixIfFlag err),
                        Handler (\(err :: DecodeError) -> fixIfFlag err)
                      ]
        else do
          doCreate <- isJust <$> lookupEnv createMissingGoldenEnv
          if doCreate
            then createGoldenfile @s settings proxy goldenFile
            else expectationFailure $ "Missing golden file: " <> goldenFile

-- | The golden files already exist. Serialize values with the same seed from
-- the golden file and compare the with the data in the golden file.
compareWithGolden ::
  forall s a.
  (GoldenSerializerConstraints s a, Arbitrary a) =>
  Settings ->
  TypeNameInfo a ->
  Proxy (s a) ->
  FilePath ->
  ComparisonFile ->
  IO ()
compareWithGolden settings typeNameInfo Proxy goldenFile comparisonFile = do
  fileContent <- readFile goldenFile
  putStrLn "before goldenSampleWithoutBody"
  goldenSampleWithoutBody :: (RandomSamples a) <- unlift @s <$> decodeIO fileContent
  putStrLn "after goldenSampleWithoutBody"
  let goldenSeed = seed goldenSampleWithoutBody
  let sampleSize = Prelude.length $ samples $ goldenSampleWithoutBody
  newSamples :: s (RandomSamples a) <- lift <$> mkRandomSamples sampleSize (Proxy :: Proxy a) goldenSeed
  whenFails (writeComparisonFile newSamples) $ do
    goldenSamples :: s (RandomSamples a) <- decodeIO fileContent
    if encode newSamples == encode goldenSamples
      then return ()
      else do
        -- fallback to testing roundtrip decoding/encoding of golden file
        putStrLn $
          "\n"
            ++ "WARNING: Encoding new random samples do not match "
            ++ goldenFile
            ++ ".\n"
            ++ "  Testing round-trip decoding/encoding of golden file."
        if encode goldenSamples == fileContent
          then return ()
          else do
            writeReencodedComparisonFile goldenSamples
            expectationFailure $ "Serialization has changed. Compare golden file with " ++ faultyReencodedFilePath ++ "."
  where
    whenFails :: forall b c. IO c -> IO b -> IO b
    whenFails = flip onException
    filePath =
      case comparisonFile of
        FaultyFile -> mkFaultyFile settings typeNameInfo
        OverwriteGoldenFile -> goldenFile
    faultyReencodedFilePath = mkFaultyReencodedFile settings typeNameInfo
    writeComparisonFile newSamples = do
      writeFile filePath (encode newSamples)
      putStrLn $
        "\n"
          ++ "INFO: Written the current encodings into "
          ++ filePath
          ++ "."
    writeReencodedComparisonFile samples = do
      writeFile faultyReencodedFilePath (encode samples)
      putStrLn $
        "\n"
          ++ "INFO: Written the reencoded goldenFile into "
          ++ faultyReencodedFilePath 
          ++ "."

-- | The golden files do not exist. Create it.
createGoldenfile :: forall s a. (Ctx s (RandomSamples a), GoldenSerializer s, Arbitrary a) => Settings -> Proxy (s a) -> FilePath -> IO ()
createGoldenfile Settings {..} Proxy goldenFile = do
  putStrLn "Create Golden File"
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
      ++ "  will compare " ++ fileType ++ " encodings with this from now on.\n"
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

-- | Create the file path to save results from a failed golden test. Optionally
-- use the module name to help avoid name collisions.  Different modules can
-- have types of the same name.
mkFaultyFile :: Settings -> TypeNameInfo a -> FilePath
mkFaultyFile Settings {..} (TypeNameInfo {typeNameTypeName, typeNameModuleName, typeNameTopDir}) =
  case unModuleName <$> typeNameModuleName of
    Nothing -> unTopDir typeNameTopDir </> unTypeName typeNameTypeName <.> "faulty" <.> fileType
    Just moduleName -> unTopDir typeNameTopDir </> moduleName </> unTypeName typeNameTypeName <.> "faulty" <.> fileType

-- | Create the file path to save results from a failed fallback golden test. Optionally
-- use the module name to help avoid name collisions.  Different modules can
-- have types of the same name.
mkFaultyReencodedFile :: Settings -> TypeNameInfo a -> FilePath
mkFaultyReencodedFile Settings {..} (TypeNameInfo {typeNameTypeName, typeNameModuleName, typeNameTopDir}) =
  case unModuleName <$> typeNameModuleName of
    Nothing -> unTopDir typeNameTopDir </> unTypeName typeNameTypeName <.> "faulty" <.> "reencoded" <.> fileType
    Just moduleName -> unTopDir typeNameTopDir </> moduleName </> unTypeName typeNameTypeName <.> "faulty" <.> "reencoded" <.> fileType

-- | Create a number of arbitrary instances of a type
-- a sample size and a random seed.
mkRandomSamples ::
  forall a.
  (Arbitrary a) =>
  Int ->
  Proxy a ->
  Int32 ->
  IO (RandomSamples a)
mkRandomSamples sampleSize Proxy rSeed = (RandomSamples rSeed) <$> generate gen
  where
    correctedSampleSize = if sampleSize <= 0 then 1 else sampleSize
    gen :: Gen [a]
    gen = setSeed (fromIntegral rSeed) $ replicateM correctedSampleSize (arbitrary :: Gen a)
