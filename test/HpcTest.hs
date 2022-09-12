module HpcTest (tests) where

import           Common                      (testConfiguration, testdataDir)
import           Control.Exception           (SomeException, catch)
import           System.FilePath             ((</>))
import           Test.Coverage.Configuration (Configuration (..))
import           Test.Coverage.Hpc
import           Test.Tasty
import           Test.Tasty.HUnit
import           Trace.Hpc.Tix

tests :: TestTree
tests = testGroup "HPC Testing" [readTixTests, readMixTests]

readTixTests :: TestTree
readTixTests = testGroup "Tix File" [
  testCase "success" $ do
      let fp = testdataDir </> "success.tix"
      tixFile <- readTixFile fp
      case tixFile of
        Just (Tix _) -> pure ()
        _            -> assertFailure "Failed to read valid tix file"
  , testCase "does not exist" $ do
      let fp = testdataDir </> "does-not-exist.tix"
      tixFile <- readTixFile fp
      case tixFile of
        Just (Tix _) -> assertFailure "Should not be a valid tix file"
        Nothing      -> pure ()
  , testCase "read exception" $ do
      let fp = testdataDir </> "bad-format.tix"
      tixFile <- readTixFile fp
      catch (case tixFile of
        Just (Tix _) -> assertFailure "Should not be a valid tix file"
        Nothing      -> assertFailure "Should not reach - Nothing") (\ (_ :: SomeException) -> pure ())

  ]

readMixTests :: TestTree
readMixTests = testGroup "Mix File" [
  testCase "success" $ do
      let Configuration{..} = testConfiguration
      tix <- getTix tixPath
      mix <- readMixFiles [mixPath] tix
      case mix of
        [] -> assertFailure "Failed to read mix files"
        _  -> pure ()
  , testCase "does not exist" $ do
      let Configuration{..} = testConfiguration
      tix <- getTix tixPath
      catch (readMixFiles ["does-not-exist"] tix >> assertFailure "Should not reach - mix files") (\ (_ :: SomeException) -> pure ())
  ]
   where
     getTix fp = do
        tixFile <- readTixFile fp
        case tixFile of
          Just t@(Tix _) -> pure t
          _              -> assertFailure "Failed to read valid tix file"
