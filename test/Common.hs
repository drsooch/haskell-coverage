-- | Testing Utilities
module Common where

import           System.FilePath     ((</>))
import           System.IO.Unsafe    (unsafePerformIO)
import           Test.Coverage
import           Test.Coverage.Hpc
import           Test.Coverage.Types
import           Test.Tasty.HUnit    (assertFailure)

testdataDir :: FilePath
testdataDir = "test/testdata"

testConfiguration :: Configuration
testConfiguration = Configuration { outputFormat = Coveralls
                                  , token        = Nothing
                                  , tixPath      = testdataDir </> "emulator/tix/emulator.tix"
                                  , mixPath      = testdataDir </> "emulator/mix"
                                  , outputFile   = Nothing
                                  }

{-# NOINLINE loadTestCoverageData #-}
loadTestCoverageData :: CoverageData
loadTestCoverageData = unsafePerformIO $ do
  eCoverageData <-  runCoverage getCoverageData testConfiguration
  case eCoverageData of
        Left e        -> assertFailure $ "Could not load Test CoverageData: " <> show e
        Right covData -> pure covData
