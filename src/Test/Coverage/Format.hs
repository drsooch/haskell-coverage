-- | Provides various Coverage Formats

module Test.Coverage.Format (CoverageFormat(..)) where

import           Test.Coverage.Format.Codecov
import           Test.Coverage.Format.Coveralls

data CoverageFormat = Coveralls
                    | Codecov
                    | Other
                    deriving Show
