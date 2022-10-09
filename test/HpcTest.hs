module HpcTest (tests) where

import           Common                     (loadTestCoverageData,
                                             testConfiguration, testdataDir)
import           Control.Exception          (SomeException, catch)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.List                  (find)
import           System.FilePath            ((<.>), (</>))
import           Test.Coverage
import           Test.Tasty
import           Test.Tasty.Golden          (goldenVsString)
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "HPC Testing" [readTixTests, readMixTests, generateCoverageListTest]

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
      mix <- readCoverageData [mixPath] tix

      case mix of
        [] -> assertFailure "Failed to read mix files"
        _  -> pure ()
  , testCase "does not exist" $ do
      let Configuration{..} = testConfiguration
      tix <- getTix tixPath
      catch (readCoverageData ["does-not-exist"] tix >> assertFailure "Should not reach - mix files") (\ (_ :: SomeException) -> pure ())
  ]
   where
     getTix fp = do
        tixFile <- readTixFile fp
        case tixFile of
          Just t@(Tix _) -> pure t
          _              -> assertFailure "Failed to read valid tix file"

goldenDir :: FilePath
goldenDir = testdataDir </> "golden/hpc"

goldenTest :: FilePath -> TestTree
goldenTest fp = goldenVsString fp (goldenDir </> mkGoldenFilePath fp) $ do
    case find (\(_, Mix mfp _ _ _ _) -> mfp == fp) loadTestCoverageData of
      Nothing -> assertFailure $ "Couldn't find File: " <> fp
      Just mc -> do
        pure $ LB.pack $ show $ generateCoverageList mc
    where
      mkGoldenFilePath fp' = map (\c -> if c == '/' then '-' else c) fp' <.> "golden"

generateCoverageListTest :: TestTree
generateCoverageListTest = testGroup "Generate Coverage List" $ map goldenTest
  [
    -- Mix File Name to match, and Number of Lines in a file
    "src/Assembler/Analyze.hs"
  , "src/Assembler/Error.hs"
  , "src/Assembler/Parser.hs"
  , "src/Assembler/Types/ASMState.hs"
  , "src/Assembler/Types/ASMStatement.hs"
  , "src/Assembler/Types/ASMTree.hs"
  , "src/Assembler/Types/Pretty.hs"
  , "src/Assembler/Utils.hs"
  , "src/Decode.hs"
  , "src/Display.hs"
  , "src/Execution.hs"
  , "src/Flags.hs"
  , "src/Instruction.hs"
  , "src/Logging.hs"
  , "src/Memory.hs"
  , "src/ProgramCounter.hs"
  , "src/Register.hs"
  , "src/Stack.hs"
  , "src/Types.hs"
  , "src/Utils.hs"
  ]
