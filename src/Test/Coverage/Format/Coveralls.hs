{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns   #-}
-- | Provides Coveralls Code Coverage Formatting

module Test.Coverage.Coveralls (formatCoveralls) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.Aeson.Types            as Aeson
import qualified Data.ByteString             as SB
import qualified Data.ByteString.Lazy        as LB
import           Data.Digest.Pure.MD5        (md5)
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap.Strict          as M
import           Data.List                   (foldl')
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           GHC.Generics                (Generic)
import           Test.Coverage.Configuration
import           Test.Coverage.Hpc


data CoverallsMetaData = CoverallsMetaData { repo_token           :: Text
                                           , service_name         :: Text
                                           , service_number       :: Maybe Text
                                           , service_job_id       :: Maybe Text
                                           , service_pull_request :: Maybe Text
                                           , source_files         :: [SourceFile]
                                           , parallel             :: Bool
                                           , flag_name            :: Maybe Text
                                           , git                  :: Maybe Text
                                           , commit_sha           :: Maybe Text
                                           , run_at               :: Maybe Text
                                           } deriving (Generic, ToJSON)

{-
        {
            "source_file": {
                "name":          required - STRING - file path for source file
                "source_digest": required - STRING - md5 digest of entire source file
                "coverage":      required - ARRAY  - Array of coverage data           --> number indicates times covered, 0 indicates no coverage, null indicates whitespace or comments
                "branches":      optional - ARRAY  - Array of branch data             --> [line number, block-number, branch-number, hits]
                "source":        optional - String - Dump of entire source contents   --> required for manual repos on enterprise
            }
        }
-}

data SourceFile = SourceFile { name          :: Text
                             , source_digest :: Text
                             , coverage      :: [LineCoverage]
                             , branches      :: Maybe [Int]
                             , source        :: Maybe Text
                             } deriving (Generic, ToJSON)

data LineCoverage = NotCoverable
                  | NotCovered
                  | Covered Int

instance ToJSON LineCoverage where
  toJSON NotCoverable = Aeson.Null
  toJSON NotCovered   = Aeson.Number 0
  toJSON (Covered c)  = Aeson.Number (fromIntegral c)


formatCoveralls :: MonadIO m => CoverageData -> m [SourceFile]
formatCoveralls covData = do
  Configuration{..} <- asks
  srcFiles <- mapM formatFile covData
  token <- fromJust <$> token
  undefined

coverallsMetaData :: MonadIO m => m CoverallsMetaData
coverallsMetaData = undefined

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
      coverage = generateCoverageList modCov (length $ T.lines source')
  pure SourceFile{..}

-- | Generate a List where the index to the list corresponds to a specific line number (index + 1)
-- the value at each index indicates the amount of hits (tix) for that line
generateCoverageList :: ModuleCoverage -> Int -> [LineCoverage]
generateCoverageList modCov fileSize = [lineToCov idx | idx <- [1..fileSize]]
  where
    coverageMap = generateCoverageMap modCov
    lineToCov lineNo = case M.lookup lineNo coverageMap of
                        Just 0   -> NotCovered
                        Just tix -> Covered tix
                        Nothing  -> NotCoverable

-- | Generate a Map with an index containing a Line Number and a value with number of hits (tix)
generateCoverageMap :: ModuleCoverage -> IntMap Int
generateCoverageMap (TixModule _ _ _ tixs, Mix _ _ _ _ mixEntries) = foldl' accumEntry M.empty (zip tixs (map fst mixEntries))
  where
    accumEntry coverage (tix, fromHpcPos -> (ls, _, _, le)) = coverage `M.union` M.fromList [(idx, fromIntegral tix) | idx <- [ls..le]]
