module Main where

import qualified CoverageTest
import qualified HpcTest
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Haskell-Coverage" [HpcTest.tests, CoverageTest.tests]
