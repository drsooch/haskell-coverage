[![Coverage Status](https://coveralls.io/repos/github/drsooch/haskell-coverage/badge.svg?branch=main)](https://coveralls.io/github/drsooch/haskell-coverage?branch=main)

[Installation](#installation)

[Usage](#usage)

[Finding your tix/mix files](#finding-your-tix-file-and-mix-path)

[Troubleshooting](#troubleshooting)

[Coverage Provider Information](#coverage-provider-information)

# Haskell Coverage

`haskell-coverage` aims to provide an up-to-date executable that translates Haskell's HPC format into various code coverage formats.

## In-Progress Formats
- `codecov` - [homepage](https://about.codecov.io/)

## Supported Formats
- `coveralls` - [homepage](https://coveralls.io/)

## Installation

``` sh
cabal install haskell-coverage
```

## Usage

`haskell-coverage` provides generic help text:

```
Usage: haskell-coverage FORMAT_TYPE [API_TOKEN] (-t|--tix-path TIX_PATH)
                        (-m|--mix-path MIX_PATH) [-o|--output-file OUTPUT_FILE]
                        [-d|--dry-run]

  Translate HPC into various Code Coverage Formats

Available options:
  FORMAT_TYPE              Code Coverage format - one of [codecov, coveralls]
  -t,--tix-path TIX_PATH   Path to tix file directory
  -m,--mix-path MIX_PATH   Path to mix file directory
  -o,--output-file OUTPUT_FILE
                           Path/name to output coverage file
  -d,--dry-run             Produce Coverage Report only (don't send to the
                           Coverage Provider)
  -h,--help                Show this help text
```

Generating coverage for coveralls would look like the following:

``` sh
haskell-coverage coveralls <API_TOKEN> -t /path/to/tix-file -m /path/to/mix-directory -o coverage.json
```

## Finding your Tix File and Mix Path

#### Cabal

These paths aren't always accurate depending on how the tix file is generated.

Assuming the following is run in a cabal project: `cabal test --enable-coverage`
The files can be found along this path:  `TIX_MIX_ROOT = /path/to/project/dist-newstyle/build/$arch/$ghc-version/package-0.1.0.0/hpc/vanilla`.
Underneath this directory you will find `mix/` and `tix/` directories.

Assuming the package name `package-0.1.0.0`, your tix file will be at `$TIX_MIX_ROOT/tix/package-0.1.0.0/package.tix`

The corresponding mix directory will be located here `$TIX_MIX_ROOT/mix/package-0.1.0.0`. **NOTE** there is a secondary directory underneath called something like `package-0.1.0.0/package-0.1.0.0-inplace`.
The `inplace` directory is what `haskell-coverage` is going to look for so make sure not to descend too far.

#### Stack

Assuming the following is run in a stack project `stack test --coverage`

The tix file can be found at this (run from the project root) `$(stack path --local-hpc-root)/package/package-test-suite-name/package.tix`

Mysteriously, the mix files are located here: `path/to/package/.stack-work/dist/$arch/$cabal-version/hpc/`.
Don't pass the full path to the mix files (should be under `package-$hash`).

## Troubleshooting

Error reporting is not particularly graceful in `haskell-coverage` yet.
Your best bet if you get failures is to run one of the `hpc` commands with similar inputs.
This generally outputs an error message that may help fix the issue.

## Coverage Provider Information

### Coveralls

Integrating `haskell-coverage` with coveralls is relatively straightforward.
Assuming you have a repository that is already connected to Coveralls, you can either:
- Provide the `API Token` to the command line argument 

OR 

- Let `haskell-coverage` query your CI environment to provide the correct information to Coveralls

Regardless of which option you choose `haskell-coverage` will integrate all possible information that it can provide to Coveralls.

The currently supported CI environments are:
- TravisCI
- Travis Pro (this is for private repos) 
  - **NOTE** You must create an environment variable `TRAVIS_PRO` as `haskell-coverage` cannot distinguish between `travis-ci` and `travis-pro` without it
- JenkinsCI
