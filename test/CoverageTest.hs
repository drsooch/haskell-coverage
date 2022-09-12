module CoverageTest (tests) where

import           Common                      (testConfiguration)
import           Test.Coverage
import           Test.Coverage.Configuration
import           Test.Coverage.Error
import           Test.Coverage.Hpc           (HpcData (..))
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Trace.Hpc.Tix

tests :: TestTree
tests = testGroup "Coverage Tests" [hpcDataTests]

hpcDataTests :: TestTree
hpcDataTests = testGroup "hpcData" [
  testCase "success" $ do
      eHpcData<- runCoverageT gatherHpcData testConfiguration
      case eHpcData of
        Left err -> assertFailure (show err)
        Right HpcData{..} -> do
          let Tix tixMod = tixData
          length tixMod @?= 20
          length mixData @?= 20
  , testCase "tix failure" $ do
       eHpcData<- runCoverageT gatherHpcData (testConfiguration { tixPath = "does-not-exist.tix" })
       case eHpcData of
         Left FailedToReadTixFile  -> pure ()
         Left FailedToReadMixFiles -> assertFailure "Should not have read any mix files"
         Right _                   -> assertFailure "Got valid HPC Data"
  , testCase "mix failure" $ do
       eHpcData<- runCoverageT gatherHpcData (testConfiguration { mixPath = "does/not/exist"})
       case eHpcData of
         Left FailedToReadMixFiles -> pure ()
         Left FailedToReadTixFile -> assertFailure "Should not have failed on tix file"
         Right _                   -> assertFailure "Got valid HPC Data"
  ]
