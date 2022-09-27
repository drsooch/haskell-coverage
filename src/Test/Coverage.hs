module Test.Coverage (
    haskellCoverage
  , getCoverageData
  ) where

import           Control.Exception       (SomeException, try)
import           Control.Monad           (when)
import           Control.Monad.Except    (throwError)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (asks)
import           Data.Aeson              (encodeFile)
import           System.Directory        (getTemporaryDirectory)
import           System.FilePath         ((</>))
import           Test.Coverage.Coveralls
import           Test.Coverage.Error
import           Test.Coverage.Hpc
import           Test.Coverage.Types

haskellCoverage :: Configuration -> IO (Either CoverageError ())
haskellCoverage = runCoverage haskellCoverage'

haskellCoverage' :: MonadCoverage m => m ()
haskellCoverage' = do
  coverageData <- getCoverageData
  formatResult <- formatCoverage coverageData
  outputFile <- asks outputFile
  case outputFile of
    Nothing -> liftIO getTemporaryDirectory >>= \dir -> liftIO $ encodeFile (dir </> "coverage.json") formatResult
    Just fp -> liftIO $ encodeFile fp formatResult

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
