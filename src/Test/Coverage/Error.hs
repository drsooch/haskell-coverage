-- | Error Functions and Data related to HPC manipulation

module Test.Coverage.Error (CoverageError(..)) where

import           Control.Exception (Exception (displayException), IOException)
import           Data.Text         (Text)
import qualified Data.Text         as T

-- | Errors related to the Coverage Monad
data CoverageError = FailedToReadTixFile
                   | FailedToReadMixFiles
                   | NoBuildInformation
                   | CodecovUnsupported
                   | IOError IOException
                   | NetworkError Text

instance Show CoverageError where
  show = \case
    FailedToReadTixFile  -> "Failed to find/read Tix file"
    FailedToReadMixFiles -> "Failed to find/read Mix files"
    NoBuildInformation   -> "Could not determine Build Information need an API token"
    CodecovUnsupported   -> "Codecov is unsupported"
    IOError e            -> displayException e
    NetworkError x       -> T.unpack x
