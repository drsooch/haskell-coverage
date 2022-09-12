module Test.Coverage (haskellCoverage, runCoverageT, gatherHpcData) where

import           Control.Exception           (SomeException, try)
import           Control.Monad               (when)
import           Control.Monad.Except        (ExceptT, MonadError (throwError),
                                              runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader        (MonadReader (..), ReaderT,
                                              runReaderT)
import           Test.Coverage.Configuration
import           Test.Coverage.Error
import           Test.Coverage.Hpc


haskellCoverage :: Configuration -> IO ()
haskellCoverage config = do
  result <- runCoverageT haskellCoverage' config
  undefined

haskellCoverage' :: MonadIO m => CoverageT m HpcData
haskellCoverage' = gatherHpcData


newtype CoverageT m a = CoverageT (ReaderT Configuration (ExceptT CoverageError m) a)
                                deriving (Functor, Applicative, Monad, MonadIO, MonadReader Configuration, MonadError CoverageError)

runCoverageT :: MonadIO m => CoverageT m a -> Configuration -> m (Either CoverageError a)
runCoverageT (CoverageT action) = runExceptT . runReaderT action

-- | Given a valid Configuration retrieve the Tix File and then collect the related Mix Files
gatherHpcData :: MonadIO m => CoverageT m HpcData
gatherHpcData = do
  Configuration{..} <- ask
  mTixFile <- readTixFile tixPath
  case mTixFile of
    Nothing      -> throwError FailedToReadTixFile
    Just tixData -> do
      eMixData <- liftIO $ (try :: IO a -> IO (Either SomeException a)) $ readMixFiles [mixPath] tixData
      case eMixData of
        Left _        -> throwError FailedToReadMixFiles
        Right mixData -> do
          when (null mixData) $ throwError FailedToReadMixFiles
          pure HpcData{..}
