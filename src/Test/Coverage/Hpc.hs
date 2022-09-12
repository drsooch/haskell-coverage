-- | Interacting with HPC related data structures and functions
module Test.Coverage.Hpc (HpcData(..), readTixFile, readMixFiles) where

import           Control.Monad.IO.Class (MonadIO (..))
import qualified Trace.Hpc.Mix          as HPC
import           Trace.Hpc.Mix          (Mix)
import qualified Trace.Hpc.Tix          as HPC
import           Trace.Hpc.Tix          (Tix (..), TixModule)

data HpcData = HpcData { tixData :: Tix
                       , mixData :: [Mix]
                       }

-- | Read a TixFile, given the relative filepath
readTixFile :: MonadIO m => FilePath -> m (Maybe Tix)
readTixFile = liftIO . HPC.readTix

-- | Read all MixFiles for a given TixFile
readMixFiles :: MonadIO m => [String] -> Tix -> m [Mix]
readMixFiles dirs (Tix tixMod) = liftIO $ mapM (readMixFile dirs) tixMod

-- | Read a single MixFile
readMixFile :: MonadIO m => [String] -> TixModule -> m Mix
readMixFile dirs = liftIO . HPC.readMix dirs . Right
