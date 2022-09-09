module Test.Coverage.Hpc (readTixFile, readMixFile) where

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Trace.Hpc.Mix          as HPC
import           Trace.Hpc.Mix          (Mix)
import qualified Trace.Hpc.Tix          as HPC
import           Trace.Hpc.Tix          (Tix, TixModule)

readTixFile :: MonadIO m => FilePath -> m (Maybe Tix)
readTixFile = liftIO . HPC.readTix

readMixFile :: MonadIO m => [String] -> Either String TixModule -> m Mix
readMixFile dirs = liftIO . HPC.readMix dirs
