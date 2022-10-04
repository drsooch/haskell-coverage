module Test.Coverage.Types (
    MonadCoverage(..)
  , Configuration(..)
  , CoverageFormatter(..)
  , CoverageT
  , Coverage
  , runCoverage
  ) where

import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Reader.Class (ask)
import           Data.Text                  (Text)
import           System.Directory           (getTemporaryDirectory)
import           System.FilePath            ((</>))
import           Test.Coverage.Error

type Coverage = CoverageT IO
type CoverageT m = ReaderT Configuration (ExceptT CoverageError m)

runCoverage ::  Coverage a -> Configuration -> IO (Either CoverageError a)
runCoverage action config = runExceptT $ runReaderT action config

data CoverageFormatter = Coveralls
                       | Codecov
                       deriving Show

data Configuration = Configuration { outputFormat :: CoverageFormatter
                                   , token        :: Maybe Text
                                   , tixPath      :: FilePath
                                   , mixPath      :: FilePath
                                   , outputFile   :: Maybe FilePath
                                   , dryRun       :: Bool
                                   } deriving Show

class (Monad m, MonadIO m, MonadReader Configuration m, MonadError CoverageError m) => MonadCoverage m where
    resolveOutputPath :: m FilePath

instance (MonadIO m, Monad m) => MonadCoverage (CoverageT m) where
    resolveOutputPath = do
      Configuration{outputFile} <- ask
      case outputFile of
        Nothing -> liftIO $ (</> "coverage.json") <$> getTemporaryDirectory
        Just fp -> pure fp
