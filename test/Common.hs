-- | Testing Utilities
module Common where

import           System.FilePath             ((</>))
import           Test.Coverage.Configuration
import           Test.Coverage.Format

testdataDir :: FilePath
testdataDir = "test/testdata"

testConfiguration :: Configuration
testConfiguration = Configuration (testdataDir </> "emulator/tix/emulator.tix") (testdataDir </> "emulator/mix") Coveralls
