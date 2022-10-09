module CoverallsTest (tests) where

import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           System.Environment                 (setEnv, unsetEnv)
import           Test.Coverage.Coveralls.CIProvider
import           Test.Tasty                         (TestName, TestTree,
                                                     testGroup, withResource)
import           Test.Tasty.HUnit                   (testCase, (@?=))

tests :: TestTree
tests = testGroup "Coveralls Testing" [travisCITest, travisProTest, jenkinsCITest]

travisCITest, travisProTest, jenkinsCITest :: TestTree
travisCITest = providerTest travisCI
-- Travis Pro will have TRAVIS set in it's env which can cause subtle bugs if we don't order our checks properly
travisProTest = withResource (setEnv "TRAVIS" "true") (const (unsetEnv "TRAVIS")) (const (providerTest travisPro))
jenkinsCITest = providerTest jenkinsCI

providerTest :: CIProvider -> TestTree
providerTest ci = testGroup (T.unpack (serviceName ci)) [
  testCIEnv "All values set" (Just (envFlag ci)) (Just (envServiceNumber ci)) (envServiceJobID ci) (Just (serviceName ci))
  -- Travis Pro will default to Travis CI when not set so we put a dumb hack here
  , testCIEnv "No Flag Set" Nothing (Just (envServiceNumber ci)) (envServiceJobID ci) (if serviceName ci == serviceName travisPro then Just (serviceName travisCI) else Nothing)
  , testCIEnv "No Number Set" (Just (envFlag ci)) Nothing (envServiceJobID ci) Nothing
  , testCIEnv "No Job ID Set" (Just (envFlag ci)) (Just (envServiceNumber ci)) Nothing (Just (serviceName ci))
  , testCIEnv "No Flag and No Number Set" Nothing Nothing (envServiceJobID ci) Nothing
  , testCIEnv "No Number and Job ID Set" (Just (envFlag ci)) Nothing Nothing Nothing
  ]

testCIEnv :: TestName -> Maybe String -> Maybe String -> Maybe String -> Maybe Text -> TestTree
testCIEnv tn flag number mJobId expected = testCase tn $ do
  actual <- withBuildEnv flag number mJobId
  actual @?= expected

withBuildEnv :: Maybe String -> Maybe String -> Maybe String -> IO (Maybe Text)
withBuildEnv flag number mJobId = do
  setEnvIf flag "ON"
  setEnvIf number "1234"
  setEnvIf mJobId "1234.1"
  serviceData <- determineServiceData
  unsetEnvIf flag
  unsetEnvIf number
  unsetEnvIf mJobId
  pure (fst3 <$> serviceData)

unsetEnvIf :: Maybe String -> IO ()
unsetEnvIf Nothing    = pure ()
unsetEnvIf (Just var) = unsetEnv var

setEnvIf :: Maybe String -> String -> IO ()
setEnvIf Nothing _      = pure ()
setEnvIf (Just var) val = setEnv var val

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
