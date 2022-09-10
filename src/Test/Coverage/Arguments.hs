-- | Command Line Arguments for Haskell-Coverage
module Test.Coverage.Arguments (Arguments(..), argParser) where

import           Data.Char            (toLower)
import           Options.Applicative  (Parser, ParserInfo, ReadM, fullDesc,
                                       help, helper, info, long, metavar,
                                       option, progDesc, readerError, short,
                                       str, strOption, (<**>))
import           Test.Coverage.Format (CoverageFormat (..))

data Arguments = Arguments { tixPath      :: FilePath
                           , mixPath      :: FilePath
                           , outputFormat :: CoverageFormat
                           } deriving Show

arguments :: Parser Arguments
arguments = Arguments <$>
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

argParser :: ParserInfo Arguments
argParser = info (arguments <**> helper) (fullDesc <> progDesc "Translate HPC into various Code Coverage Formats")

coverageFormatParser :: ReadM CoverageFormat
coverageFormatParser = map toLower <$> str >>= \case
  "coveralls" -> pure Coveralls
  "codecov" -> pure Codecov
  "other" -> pure Other
  _ -> readerError "Invalid Coverage Format must be one of: coveralls, codecov"
