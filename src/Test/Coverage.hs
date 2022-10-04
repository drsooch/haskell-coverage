module Test.Coverage (
    haskellCoverage
  , getCoverageData
  ) where

import           Control.Exception       (IOException, SomeException, try)
import           Control.Monad           (unless, when)
import           Control.Monad.Except    (MonadError, throwError)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (ask, asks)
import           Data.Aeson              (encodeFile)
import           Test.Coverage.Coveralls
import           Test.Coverage.Error
import           Test.Coverage.Hpc
import           Test.Coverage.Types

-- | Attempt to convert HPC to a Coverage Provider and either send this report to the Provider or write it to a file
haskellCoverage :: Configuration -> IO (Either CoverageError ())
haskellCoverage = runCoverage haskellCoverage'

haskellCoverage' :: MonadCoverage m => m ()
haskellCoverage' = do
  coverageData <- getCoverageData
  coverageReport <- formatCoverage coverageData
  outputFile <- resolveOutputPath
  writeCoverageReport outputFile coverageReport
  Configuration{dryRun} <- ask
  unless dryRun $ sendCoverageReport outputFile

-- | Send a CoverageReport to a Coverage Provider
sendCoverageReport :: MonadCoverage m => FilePath -> m ()
sendCoverageReport fp = asks outputFormat >>= \case
  Coveralls -> sendReportToCoveralls fp
  Codecov   -> throwError CodecovUnsupported

-- | Encode and Output the Coverage Report
writeCoverageReport :: (MonadIO m, MonadError CoverageError m) => FilePath -> CoverallsMetaData -> m ()
writeCoverageReport fp cmd = do
  eResult <- liftIO $ try $ encodeFile fp cmd
  case eResult of
    Left (e :: IOException) -> throwError $ IOError e
    Right _                 -> pure ()

-- | Read the Tix File from the provided path
getTixData :: MonadCoverage m => m Tix
getTixData = asks tixPath >>= readTixFile >>= \case
  Nothing  -> throwError FailedToReadTixFile
  Just tix -> pure tix

-- | Find and Parse a Tix File and combine that information with a MixFile for each module
getCoverageData :: MonadCoverage m => m CoverageData
getCoverageData = do
  tixData <- getTixData
  mixPath <- asks mixPath
  -- annoyingly reading mix files throws errors
  eCoverageData <- liftIO $ (try :: IO a -> IO (Either SomeException a)) $ readCoverageData [mixPath] tixData
  case eCoverageData of
    Left _             -> throwError FailedToReadMixFiles
    Right coverageData -> do
      when (null coverageData) $ throwError FailedToReadMixFiles
      pure coverageData

formatCoverage :: MonadCoverage m => CoverageData -> m CoverallsMetaData
formatCoverage coverageData = asks outputFormat >>= \case
  Coveralls -> formatCoveralls coverageData
  Codecov   -> throwError CodecovUnsupported
