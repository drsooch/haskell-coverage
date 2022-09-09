module Main where

import           Options.Applicative
import           Test.Coverage.Arguments (argParser)

main :: IO ()
main = do
  args <- execParser argParser
  print args
  -- find out if this is a stack or cabal package
  print "Stack or Cabal"
  -- find the tix file(s)
  print "Tix"
  -- find the mix file(s)
  print "mix"
  -- find out what format we want to output
  print "output format"
  -- find out where we output the file
  -- either as a file or http request to a server (codecov or coveralls for example)
  print "where to send"
