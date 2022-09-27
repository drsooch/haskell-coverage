module CoverageTest (tests) where

import           Common              (testConfiguration)
import           Test.Coverage
import           Test.Coverage.Error
import           Test.Coverage.Types
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Coverage Tests" [hpcTests]

hpcTests :: TestTree
hpcTests = testGroup "coverageData" [
  testCase "success" $ do
      eCoverageData <- runCoverage getCoverageData testConfiguration
      case eCoverageData of
        Left err      -> assertFailure (show err)
        Right covData -> length covData @?= 20
  , testCase "tix failure" $ do
       eHpcData<- runCoverage getCoverageData (testConfiguration { tixPath = "does-not-exist.tix" })
       case eHpcData of
         Left FailedToReadTixFile  -> pure ()
         Left FailedToReadMixFiles -> assertFailure "Should not have read any mix files"
         Left _ -> assertFailure "Invalid Failure"
         Right _                   -> assertFailure "Got valid HPC Data"
  , testCase "mix failure" $ do
       eHpcData<- runCoverage getCoverageData (testConfiguration { mixPath = "does/not/exist"})
       case eHpcData of
         Left FailedToReadMixFiles -> pure ()
         Left FailedToReadTixFile -> assertFailure "Should not have failed on tix file"
         Left _ -> assertFailure "Invalid Failure"
         Right _                   -> assertFailure "Got valid HPC Data"
  ]
