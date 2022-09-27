-- | Provides Codecov Coverage Formatting

module Test.Coverage.Codecov (formatCodecov) where

import           Control.Monad.Except (throwError)
import           Test.Coverage.Error
import           Test.Coverage.Hpc
import           Test.Coverage.Types

formatCodecov :: MonadCoverage m => CoverageData -> m a
formatCodecov _ = throwError CodecovUnsupported
