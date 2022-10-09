{-# LANGUAGE DeriveAnyClass #-}
-- | Provides Coveralls Code Coverage Formatting

module Test.Coverage.Coveralls (
    formatCoveralls
  , sendReportToCoveralls
  , CoverallsMetaData(..)
  , SourceFile(..)
  ) where

import           Control.Monad                         (unless)
import           Control.Monad.Except                  (MonadError (throwError))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Reader                  (asks)
import           Data.Aeson                            (ToJSON)
import qualified Data.ByteString                       as SB
import qualified Data.ByteString.Lazy                  as LB
import           Data.Digest.Pure.MD5                  (md5)
import           Data.Maybe                            (isJust)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           GHC.Generics                          (Generic)
import           Network.HTTP.Client                   (httpLbs, newManager,
                                                        parseRequest_,
                                                        responseStatus)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partFile)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.HTTP.Types                    (statusIsSuccessful)
import           Test.Coverage.Coveralls.CIProvider
import           Test.Coverage.Coveralls.Git
import           Test.Coverage.Error
import           Test.Coverage.Hpc
import           Test.Coverage.Types

{-
        {
           "repo_token":           required - STRING - API Token for Coveralls
           "service_name":         required - STRING - CI or service environment where test was run
           "service_number":       optional - STRING - Build Number
           "service_job_id":       optional - STRING - Build Identifier (unique)
           "service_pull_request": optional - STRING - Pull Request ID
           "source_files":         optional - ARRAY  - Array of SourceFile
           "parallel":             optional - BOOL   - Build won't be considered done until a webhook was sent to coveralls
           "flag_name":            optional - STRING - Job will be named this flag
           "git":                  optional - STRING - Git Hash to display extra data to user
           "commit_sha":           optional - STRING - Commit being built (overrides git parameter)
           "run_at":               optional - STRING - Timestamp of job being run
        }
-}

-- | Top Level of the Coveralls API
data CoverallsMetaData = CoverallsMetaData { repo_token           :: Text
                                           , service_name         :: Text
                                           , service_number       :: Maybe Text
                                           , service_job_id       :: Maybe Text
                                           , service_pull_request :: Maybe Text
                                           , source_files         :: [SourceFile]
                                           , parallel             :: Maybe Bool
                                           , flag_name            :: Maybe Text
                                           , git                  :: Maybe GitMetaData
                                           , commit_sha           :: Maybe Text
                                           , run_at               :: Maybe Text
                                           } deriving (Generic, ToJSON)

defaultCoverallsMetaData :: CoverallsMetaData
defaultCoverallsMetaData = CoverallsMetaData { repo_token           = ""
                                             , service_name         = ""
                                             , service_number       = Nothing
                                             , service_job_id       = Nothing
                                             , service_pull_request = Nothing
                                             , source_files         = []
                                             , parallel             = Nothing
                                             , flag_name            = Nothing
                                             , git                  = Nothing
                                             , commit_sha           = Nothing
                                             , run_at               = Nothing
                                             }

{-
        {
            "source_file": {
                "name":          required - STRING - file path for source file
                "source_digest": required - STRING - md5 digest of entire source file
                "coverage":      required - ARRAY  - Array of coverage data           --> number indicates times covered, 0 indicates no coverage, null indicates whitespace or comments
                "branches":      optional - ARRAY  - Array of branch data             --> [line number, block-number, branch-number, hits]
                "source":        optional - STRING - Dump of entire source contents   --> required for manual repos on enterprise
            }
        }
-}

-- | Individual Haskell Source File with coverage data
data SourceFile = SourceFile { name          :: Text
                             , source_digest :: Text
                             , coverage      :: [LineCoverage]
                             , branches      :: Maybe [Int]
                             , source        :: Maybe Text
                             } deriving (Generic, ToJSON)

-- | Send Coverage data through the Coveralls API
sendReportToCoveralls :: (MonadIO m, MonadError CoverageError m) => FilePath -> m ()
sendReportToCoveralls fp = do
  manager <- liftIO $ newManager tlsManagerSettings
  let request = parseRequest_ "POST https://coveralls.io/api/v1/jobs"
  request'  <- formDataBody [partFile "json_file" fp] request
  liftIO $ print request'
  response <- liftIO $ httpLbs request' manager
  unless (statusIsSuccessful $ responseStatus response) $ throwError $ NetworkError (T.pack $ show $ responseStatus response)

-- | Construct a Coveralls API record
formatCoveralls :: MonadCoverage m => CoverageData -> m CoverallsMetaData
formatCoveralls covData = do
  source_files <- mapM formatFile covData
  metaData <- coverallsMetaData
  pure $ metaData { source_files = source_files }

-- | Translate a single ModuleCoverage into a SourceFile
-- This function does a lot of text manipulation (conversion from ByteString to String to Text etc)
-- and room for improvement is available
formatFile :: MonadIO m => ModuleCoverage -> m SourceFile
formatFile modCov@(_, Mix fp _ _ _ _) = do
  fileContents <- liftIO $ LB.readFile fp
  -- Lazy ByteString -> String -> Text (OH MY!)
  -- md5 only accepts LazyByteString
  let source_digest = T.pack $ show $ md5 fileContents
      branches = Nothing
      name = T.pack fp
      source' = T.decodeUtf8Lenient $ SB.toStrict fileContents
      source = Just source'
      coverage = generateCoverageList modCov
  pure SourceFile{..}

-- | Generate metadata for Coveralls
coverallsMetaData :: MonadCoverage m => m CoverallsMetaData
coverallsMetaData = do
  mToken <- asks token
  cmd <- case mToken of
            Nothing    -> pure defaultCoverallsMetaData
            Just token -> pure $ defaultCoverallsMetaData { repo_token = token }
  cmd' <- addServiceData cmd
  cmd'' <- addGitMetaData cmd'
  unless (hasRequiredFields cmd'') $ throwError NoBuildInformation
  pure cmd''

-- | Query the build environment and add any CI metadata
addServiceData :: MonadIO m => CoverallsMetaData -> m CoverallsMetaData
addServiceData cmd = liftIO $ do
  mServiceData <- determineServiceData
  case mServiceData of
    Nothing -> pure cmd
    Just (service_name, service_number, service_job_id) -> pure $ cmd { service_name = service_name
                                                                      , service_number = Just service_number
                                                                      , service_job_id = service_job_id}

-- | Query the build environment and add any Git metadata
addGitMetaData :: MonadIO m => CoverallsMetaData -> m CoverallsMetaData
addGitMetaData cmd = gitMetaData >>= \git -> pure cmd { git = git }

-- | Coveralls accepts EITHER a repo_token OR service_name and service_number
-- We can mix and match the combination of the two so all we need is to verify at least ONE
-- of those properties is true
hasRequiredFields :: CoverallsMetaData -> Bool
hasRequiredFields CoverallsMetaData{repo_token} | not (T.null repo_token) = True
hasRequiredFields CoverallsMetaData{service_name, service_number} = isJust service_number && not (T.null service_name)
