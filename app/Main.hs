module Main where

import           Options.Applicative
import           Test.Coverage               (haskellCoverage)
import           Test.Coverage.Configuration

main :: IO ()
main = do
  config <- execParser configurationParser
  print config
  result <- haskellCoverage config
  case result of
    Left err -> print err
    _        -> pure ()
