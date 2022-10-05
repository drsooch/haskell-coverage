module Test.Coverage.Types (
    MonadCoverage(..)
  , Configuration(..)
  , CoverageFormatter(..)
  , CoverageT
  , Coverage
  , runCoverageT
  ) where

import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Reader.Class (ask)
import           Data.Text                  (Text)
import           System.Directory           (getTemporaryDirectory)
import           System.FilePath            ((</>))
import           Test.Coverage.Error

-- | CoverageT monad with IO as the base monad
type Coverage = CoverageT IO

-- | MonadTransformer that implements MonadCoverage
type CoverageT m = ReaderT Configuration (ExceptT CoverageError m)

-- | Run an action in the Coverage Transformer monad and produce a result
runCoverageT :: CoverageT m a -> Configuration -> m (Either CoverageError a)
runCoverageT action = runExceptT . runReaderT action

data CoverageFormatter = Coveralls
                       | Codecov
                       deriving Show

data Configuration = Configuration { outputFormat :: CoverageFormatter -- ^ Which API format to produce
                                   , token        :: Maybe Text        -- ^ API Token to use (if not given and required the program will fail)
                                   , tixPath      :: FilePath          -- ^ Path to the tix file
                                   , mixPath      :: FilePath          -- ^ Directory path to the mix files
                                   , outputFile   :: Maybe FilePath    -- ^ Path to output the coverage report (defaults to /tmp/coverage.json)
                                   , dryRun       :: Bool              -- ^ If enabled, the report will not be sent to the Provider
                                   } deriving Show

-- | Coverage Monad which is mostly a wrapper around specific constraints
class (Monad m, MonadIO m, MonadReader Configuration m, MonadError CoverageError m) => MonadCoverage m where
    resolveOutputPath :: m FilePath

instance MonadIO m => MonadCoverage (CoverageT m) where
    resolveOutputPath = do
      Configuration{outputFile} <- ask
      case outputFile of
        Nothing -> liftIO $ (</> "coverage.json") <$> getTemporaryDirectory
        Just fp -> pure fp
