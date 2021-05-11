{-# LANGUAGE OverloadedStrings #-}

module Test.Cereal.GenericSpecsSpec where

import Data.Serialize (encode)
import System.Environment
import qualified Data.ByteString as BS
import Data.Proxy
import System.Directory
import Test.Cereal.GenericSpecs
import Test.Cereal.Internal.Utils (RandomMismatchOption (..), RandomSamples(..), createMissingGoldenEnv, recreateBrokenGoldenEnv)
import Test.Hspec
-- various iterations of a Product and Sum Type and their serializations
import qualified Test.Types as T
import qualified Test.Types.AlteredSelector as TAS
import qualified Test.Types.BrokenSerialization as TBS
import qualified Test.Types.MismatchedToAndFromSerialization as MTFS
import qualified Test.Types.NewSelector as TNS
import Test.Utils

unsetAllEnv :: IO ()
unsetAllEnv = do
  unsetEnv createMissingGoldenEnv
  unsetEnv recreateBrokenGoldenEnv

setCreateMissingGoldenEnv :: IO ()
setCreateMissingGoldenEnv = 
  setEnv createMissingGoldenEnv "1"

spec :: Spec
spec = before unsetAllEnv $ do
  describe "Test.Cereal.GenericSpecs: roundtripSpecs" $ do
    it "should pass when put and get are defined appropriately" $ do
      shouldProduceFailures 0 $ roundtripSpecs (Proxy :: Proxy T.Person)

    it "should fail when put and get definitions do not match" $ do
      shouldProduceFailures 1 $ roundtripSpecs (Proxy :: Proxy MTFS.Person)

  describe "Test.Cereal.GenericSpecs: roundtripADTSpecs" $ do
    it "should pass when put and get are defined appropriately" $ do
      shouldProduceFailures 0 $ roundtripADTSpecs (Proxy :: Proxy T.Person)

    it "should fail when put and get definitions do not match" $ do
      shouldProduceFailures 1 $ roundtripADTSpecs (Proxy :: Proxy MTFS.Person)

  describe "Test.Cereal.GenericSpecs: goldenSpecs" $ do
    it "create golden test files" $ do
      setCreateMissingGoldenEnv
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenSpecs defaultSettings (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Person.bin" `shouldReturn` True
      doesFileExist "golden/SumType.bin" `shouldReturn` True

    it "create golden test files" $ do
      setCreateMissingGoldenEnv
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenSpecs (defaultSettings {useModuleNameAsSubDirectory = True}) (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenSpecs (defaultSettings {useModuleNameAsSubDirectory = True}) (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Test.Types/Person.bin" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType.bin" `shouldReturn` True

    it "create golden test files in a user defined directory" $ do
      setCreateMissingGoldenEnv
      let topDir = "bin-tests"
      -- clean up previously existing user defined folder
      bg <- doesDirectoryExist topDir
      if bg
        then removeDirectoryRecursive topDir
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.SumType)

      doesFileExist "bin-tests/Person.bin" `shouldReturn` True
      doesFileExist "bin-tests/SumType.bin" `shouldReturn` True

    it "goldenADTSpecs should pass for existing golden files in which model types and serialization have not changed" $ do
      setCreateMissingGoldenEnv
      shouldProduceFailures 0 $ do
        goldenSpecs defaultSettings (Proxy :: Proxy T.Person)
        goldenSpecs defaultSettings (Proxy :: Proxy T.SumType)

    it "goldenADTSpecs for types which have changed the values of put or get keys should fail to match the goldenFiles" $ do
      shouldProduceFailures 1 $goldenSpecs defaultSettings (Proxy :: Proxy TBS.Person)

    it "goldenADTSpecs for types which have changed the values of put or get keys should fail to match the goldenFiles" $ do
      shouldProduceFailures 1 $ goldenSpecs defaultSettings (Proxy :: Proxy TNS.Person)

    it "goldenADTSpecs for types which have altered the name of the selector and using generic implementation of put and get should fail to match the goldenFiles" $ do
      shouldProduceFailures 1 $ goldenSpecs defaultSettings (Proxy :: Proxy TAS.Person)

  describe "Test.Cereal.GenericSpecs: goldenADTSpecs" $ do
    it "create golden test files" $ do
      setCreateMissingGoldenEnv
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Person/Person.bin" `shouldReturn` True
      doesFileExist "golden/SumType/SumType1.bin" `shouldReturn` True
      doesFileExist "golden/SumType/SumType2.bin" `shouldReturn` True
      doesFileExist "golden/SumType/SumType3.bin" `shouldReturn` True

    it "create golden test files in a sub directory using the module name" $ do
      setCreateMissingGoldenEnv
      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs defaultSettings {useModuleNameAsSubDirectory = True} (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs defaultSettings {useModuleNameAsSubDirectory = True} (Proxy :: Proxy T.SumType)

      doesFileExist "golden/Test.Types/Person/Person.bin" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType1.bin" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType2.bin" `shouldReturn` True
      doesFileExist "golden/Test.Types/SumType/SumType3.bin" `shouldReturn` True

    it "create golden test files in a user defined directory" $ do
      setCreateMissingGoldenEnv
      let topDir = "bin-tests"
      -- clean up previously existing user defined folder
      bg <- doesDirectoryExist topDir
      if bg
        then removeDirectoryRecursive topDir
        else return ()

      -- files for Person and SumType do not exist
      -- create them by running goldenADTSpecs
      _ <- hspecSilently $ goldenADTSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.Person)
      _ <- hspecSilently $ goldenADTSpecs (defaultSettings {goldenDirectoryOption = CustomDirectoryName topDir}) (Proxy :: Proxy T.SumType)

      doesFileExist "bin-tests/Person/Person.bin" `shouldReturn` True
      doesFileExist "bin-tests/SumType/SumType1.bin" `shouldReturn` True
      doesFileExist "bin-tests/SumType/SumType2.bin" `shouldReturn` True
      doesFileExist "bin-tests/SumType/SumType3.bin" `shouldReturn` True

    it "goldenADTSpecs should pass for existing golden files in which model types and serialization have not changed" $ do
      setCreateMissingGoldenEnv
      shouldProduceFailures 0 $ do
        goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)
        goldenADTSpecs defaultSettings (Proxy :: Proxy T.SumType)

    it "goldenADTSpecs for types which have changed the values of put or get keys should fail to match the goldenFiles" $ do
      shouldProduceFailures 1 $ goldenADTSpecs defaultSettings (Proxy :: Proxy TBS.Person)

    it "goldenADTSpecs for types which have changed the values of put or get keys should fail to match the goldenFiles" $ do
      shouldProduceFailures 1 $ goldenADTSpecs defaultSettings (Proxy :: Proxy TNS.Person)

    it "goldenADTSpecs for types which have altered the name of the selector and using generic implementation of put and get should fail to match the goldenFiles" $ do
      shouldProduceFailures 1 $ goldenADTSpecs defaultSettings (Proxy :: Proxy TAS.Person)

    let goldenByteIdentical = encode $ RandomSamples 41 [T.Person "abc" 1, T.Person "def" 2]

    it "different random seed but byte-for-byte identical should pass (default setting)" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- directly create golden file for tests
      createDirectoryIfMissing True "golden/Person"
      BS.writeFile "golden/Person/Person.bin" goldenByteIdentical

      shouldProduceFailures 0 $ goldenADTSpecs defaultSettings (Proxy :: Proxy T.Person)

    it "different random seed but byte-for-byte identical should fail (with custom setting)" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else return ()

      -- directly create golden file for tests
      createDirectoryIfMissing True "golden/Person"
      BS.writeFile "golden/Person/Person.bin" goldenByteIdentical

      let customSettings = defaultSettings {randomMismatchOption = RandomMismatchError}
      shouldProduceFailures 1 $ goldenADTSpecs customSettings (Proxy :: Proxy T.Person)

  describe "mkGoldenFileForType" $ do
    it "create a single file in a dir for a Product type" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else pure ()

      mkGoldenFileForType 10 (Proxy :: Proxy T.Person) "golden"
      doesFileExist "golden/Person/Person.bin" `shouldReturn` True

    it "create a file for each constructor in a dir for a Sum type" $ do
      -- clean up previously existing golden folder
      bg <- doesDirectoryExist "golden"
      if bg
        then removeDirectoryRecursive "golden"
        else pure ()

      mkGoldenFileForType 10 (Proxy :: Proxy T.SumType) "golden"
      doesFileExist "golden/SumType/SumType1.bin" `shouldReturn` True
      doesFileExist "golden/SumType/SumType2.bin" `shouldReturn` True
      doesFileExist "golden/SumType/SumType3.bin" `shouldReturn` True

main :: IO ()
main = 
  hspec spec
