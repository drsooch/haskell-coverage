-- | Configuration for Haskell-Coverage
module Test.Coverage.Configuration (
  configurationParser
  ) where

import           Data.Char           (toLower)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Options.Applicative (Parser, ParserInfo, ReadM, argument,
                                      fullDesc, help, helper, info, long,
                                      metavar, option, progDesc, readerError,
                                      short, str, strOption, switch, value,
                                      (<**>))
import           Test.Coverage.Types

-- | Command Line Parser for a Configuration
configuration :: Parser Configuration
configuration = Configuration <$>
  argument coverageFormatParser
        ( metavar "FORMAT_TYPE"
        <> help "Code Coverage format - one of [codecov, coveralls]")
  <*> argument apiTokenParser (metavar "API_TOKEN" <> value Nothing)
  <*> strOption
        ( long "tix-path"
        <> short 't'
        <> metavar "TIX_PATH"
        <> help "Path to tix file directory")
  <*> strOption
        ( long "mix-path"
        <> short 'm'
        <> metavar "MIX_PATH"
        <> help "Path to mix file directory")
  <*> option outputPathParser
        ( long "output-file"
        <> short 'o'
        <> metavar "OUTPUT_FILE"
        <> value Nothing
        <> help "Path/name to output coverage file")
  <*> switch
        (long "dry-run"
        <> short 'd'
        <> help "Produce Coverage Report only (don't send to the Coverage Provider)")

configurationParser :: ParserInfo Configuration
configurationParser = info (configuration <**> helper) (fullDesc <> progDesc "Translate HPC into various Code Coverage Formats")

apiTokenParser :: ReadM (Maybe Text)
apiTokenParser = Just . T.pack <$> str

outputPathParser :: ReadM (Maybe String)
outputPathParser = Just <$> str

coverageFormatParser ::  ReadM CoverageFormatter
coverageFormatParser = str >>= \s ->
  case map toLower s of
    "coveralls" -> pure Coveralls
    "codecov" -> pure Codecov
    _ -> readerError "Invalid Coverage Format must be one of: [coveralls, codecov]"
