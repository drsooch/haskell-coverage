module Test.Coverage.Coveralls.CIProvider where
import           Control.Applicative    (asum)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.Environment     (lookupEnv)

-- This is required and is always hard-coded
type ServiceName = Text
-- This is required and should always exist in the environment
type ServiceNumber = Text
-- This may not exist depending on the CI
type ServiceJobID = Maybe Text
type ServiceData = Maybe (ServiceName, ServiceNumber, ServiceJobID)

data CIProvider = CIProvider { envFlag          :: String
                             , envServiceNumber :: String
                             , envServiceJobID  :: Maybe String
                             , serviceName      :: Text
                             }

providers :: [CIProvider]
providers = [travisCI, travisPro, jenkinsCI]

travisCI, travisPro, jenkinsCI :: CIProvider
travisCI = CIProvider "TRAVIS" "TRAVIS_BUILD_NUMBER" (Just "TRAVIS_BUILD_ID") "travis-ci"
travisPro = CIProvider "TRAVIS_PRO" "TRAVIS_BUILD_NUMBER" (Just "TRAVIS_BUILD_ID") "travis-pro"
jenkinsCI = CIProvider "JENKINS_HOME" "BUILD_ID" Nothing "jenkins"

-- | Test all supported CI providers and return the first one we match on
determineServiceData :: MonadIO m => m ServiceData
determineServiceData = asum <$> mapM tryProvider providers

-- | Query an environment to determine if we are in a Specific CI
-- if we are in a valid CI then query it's environment for the metadata
tryProvider :: MonadIO m => CIProvider -> m ServiceData
tryProvider CIProvider{..} = liftIO $ do
    mEnvVal <- lookupEnv envFlag
    case mEnvVal of
        Nothing -> pure Nothing
        Just _ -> do
            mServiceNumber <- fmap T.pack <$> lookupEnv envServiceNumber
            mServiceJobID <- maybe (pure Nothing) (fmap (fmap T.pack) . lookupEnv) envServiceJobID
            -- There is an implicit expectation that serviceNumber exists in the build environment
            -- if it doesn't exist then don't return anything
            case mServiceNumber of
              Nothing            -> pure Nothing
              Just serviceNumber -> pure $ Just (serviceName, serviceNumber, mServiceJobID)
