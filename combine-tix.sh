#! /bin/sh

##### NOTE: This assumes the test-suite, exe, and lib are build with coverage enabled

VERSION=haskell-coverage-0.1.0.0

CABAL_DIST_PREFIX=dist-newstyle/build/x86_64-linux/ghc-9.2.4/$VERSION

TEST_SUITE_NAME=haskell-coverage-test
TEST_EXE_LOC=$CABAL_DIST_PREFIX/t/$TEST_SUITE_NAME/build/$TEST_SUITE_NAME/$TEST_SUITE_NAME

EXE_NAME=haskell-coverage
EXE_LOC=$CABAL_DIST_PREFIX/x/$EXE_NAME/build/$EXE_NAME/$EXE_NAME

TEST_TIX=haskell-coverage-test.tix
EXE_TIX=haskell-coverage.tix
COMBINED_TIX=haskell-coverage-combined.tix

MIX_PATH=$CABAL_DIST_PREFIX/hpc/vanilla/mix/$VERSION

# Run the tests via the test executable
$TEST_EXE_LOC

if [ $? -ne 0 ]; then
    exit 1
fi

# clean out test modules
hpc map --exclude=Main --exclude=CoverageTest --exclude=HpcTest --exclude=Common --exclude=CoverallsTest --output=$TEST_TIX $TEST_TIX

# run the executable on the test tix file
$EXE_LOC coveralls "$1" -t $TEST_TIX -m $MIX_PATH --dry-run

# Remove app/Main.hs module
hpc map --exclude=Main --output=$EXE_TIX $EXE_TIX

# merge the two tix files to complete a single one
hpc combine --output=$COMBINED_TIX $EXE_TIX $TEST_TIX

# run the executbale one more time to generate proper coverage report
$EXE_LOC coveralls "$1" -o coverage.json -t $COMBINED_TIX -m $MIX_PATH
