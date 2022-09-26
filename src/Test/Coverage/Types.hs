module Test.Coverage.Types (MonadCoverage, Configuration(..), CoverageFormatter(..), CoverageT, Coverage, runCoverage) where

import           Control.Monad.Except   (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Data.Text              (Text)
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
                                   } deriving Show

class (Monad m, MonadIO m, MonadReader Configuration m, MonadError CoverageError m) => MonadCoverage m

instance (MonadIO m, Monad m) => MonadCoverage (CoverageT m)
