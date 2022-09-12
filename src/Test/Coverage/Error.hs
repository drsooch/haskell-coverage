-- | Error Functions and Data related to HPC manipulation

module Test.Coverage.Error (CoverageError(..)) where

data CoverageError = FailedToReadTixFile
                   | FailedToReadMixFiles

instance Show CoverageError where
  show = \case
    FailedToReadTixFile  -> "Failed to find/read Tix file"
    FailedToReadMixFiles -> "Failed to find/read Mix files"
