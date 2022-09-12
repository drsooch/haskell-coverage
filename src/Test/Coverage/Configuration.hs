-- | Configuration for Haskell-Coverage
module Test.Coverage.Configuration (Configuration(..), configurationParser) where

import           Data.Char            (toLower)
import           Options.Applicative  (Parser, ParserInfo, ReadM, fullDesc,
                                       help, helper, info, long, metavar,
                                       option, progDesc, readerError, short,
                                       str, strOption, (<**>))
import           Test.Coverage.Format (CoverageFormat (..))

data Configuration = Configuration { tixPath      :: FilePath
                                   , mixPath      :: FilePath
                                   , outputFormat :: CoverageFormat
                                   } deriving Show

configuration :: Parser Configuration
configuration = Configuration <$>
  strOption
        ( long "tix-path"
        <> short 't'
        <> metavar "TIX_PATH"
        <> help "Path to tix file directory")
  <*> strOption
        ( long "mix-path"
        <> short 'm'
        <> metavar "MIX_PATH"
        <> help "Path to mix file directory")
  <*> option coverageFormatParser
        ( long "format-type"
        <> short 'f'
        <> metavar "FORMAT_TYPE"
        <> help "Code Coverage format")

configurationParser :: ParserInfo Configuration
configurationParser = info (configuration <**> helper) (fullDesc <> progDesc "Translate HPC into various Code Coverage Formats")

coverageFormatParser :: ReadM CoverageFormat
coverageFormatParser = map toLower <$> str >>= \case
  "coveralls" -> pure Coveralls
  "codecov" -> pure Codecov
  "other" -> pure Other
  _ -> readerError "Invalid Coverage Format must be one of: coveralls, codecov"
