{-# LANGUAGE ViewPatterns #-}
-- | Interacting with HPC related data structures and functions
module Test.Coverage.Hpc (
    CoverageData
  , LineCoverage
  , ModuleCoverage
  , generateCoverageList
  , generateCoverageMap
  , readTixFile
  , readCoverageData
  , module HPC
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (ToJSON (..))
import           Data.Aeson.Types       (Value (..))
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as M
import           Data.List              (foldl')
import           Trace.Hpc.Mix          as HPC
import           Trace.Hpc.Tix          as HPC
import           Trace.Hpc.Util         as HPC

type CoverageData = [(TixModule, Mix)]
type ModuleCoverage = (TixModule, Mix)

data LineCoverage = Ignored
                  | NotCovered
                  | Covered Int
                  deriving (Eq, Show)

instance ToJSON LineCoverage where
  toJSON Ignored     = Null
  toJSON NotCovered  = Number 0
  toJSON (Covered c) = Number (fromIntegral c)

-- | Read a TixFile, given the relative filepath
readTixFile :: MonadIO m => FilePath -> m (Maybe Tix)
readTixFile = liftIO . HPC.readTix

-- | Read all MixFiles for a given TixFile
readCoverageData :: MonadIO m => [String] -> Tix -> m CoverageData
readCoverageData dirs (Tix tixMod) = liftIO $ mapM (readMixFile dirs) tixMod

-- | Read a single MixFile
readMixFile :: MonadIO m => [String] -> TixModule -> m ModuleCoverage
readMixFile dirs tixMod = do
  mixData <- liftIO $ HPC.readMix dirs $ Right tixMod
  pure (tixMod, mixData)

-- | Generate a List where the index to the list corresponds to a specific line number (index + 1)
-- the value at each index indicates the amount of hits (tix) for that line
generateCoverageList :: ModuleCoverage -> [LineCoverage]
generateCoverageList modCov = map lineToCov lineIndices
  where
    lineIndices = case M.lookupMax coverageMap of
                    Just (fs, _) -> [1..fs]
                    Nothing      -> []
    coverageMap = generateCoverageMap modCov
    lineToCov lineNo = case M.lookup lineNo coverageMap of
                        Just 0   -> NotCovered
                        Just tix -> Covered tix
                        Nothing  -> Ignored

-- | Generate a Map with an index containing a Line Number and a value with number of hits (tix)
generateCoverageMap :: ModuleCoverage -> IntMap Int
generateCoverageMap (TixModule _ _ _ tixs, Mix _ _ _ _ mixEntries) = foldl' accumEntry M.empty (zip tixs mixEntries)
  where
    accumEntry coverage (tix, fromHpcPos . fst -> (ls, _, le, _)) = coverage `M.union` M.fromList [(idx, fromIntegral tix) | idx <- [ls..le]]
