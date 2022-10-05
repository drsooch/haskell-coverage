{-# LANGUAGE DeriveAnyClass #-}
module Test.Coverage.Git (
    GitMetaData
  , gitMetaData
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (ToJSON)
import qualified Data.ByteString        as SB
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           GHC.Generics           (Generic)
import           System.Process.Typed   (ExitCode (..), readProcessStdout,
                                         shell)

{-
        "git": {
            "head": {
                "id": "b31f08d07ae564b08237e5a336e478b24ccc4a65",
                "author_name": "Nick Merwin",
                "author_email": "...",
                "committer_name": "Nick Merwin",
                "committer_email": "...",
                "message": "version bump"
            },
            "branch": "master",
            "remotes": [
                {
                    "name": "origin",
                    "url": "git@github.com:lemurheavy/coveralls-ruby.git"
                }
            ]
        }
-}

-- | Top Level Git Information specific to the current project
data GitMetaData = GitMetaData { head    :: GitHead
                               , branch  :: Text
                               , remotes :: [GitRemote]
                               } deriving (Generic, ToJSON)

-- | Git information for current commit
data GitHead = GitHead { id              :: Text
                       , author_name     :: Text
                       , author_email    :: Text
                       , committer_name  :: Text
                       , committer_email :: Text
                       , message         :: Text
                       } deriving (Generic, ToJSON)

-- | List of Git Remotes
data GitRemote = GitRemote { name :: Text
                           , url  :: Text
                           } deriving (Generic, ToJSON)

-- TODO: Make this work with CIs as well

-- | Produce GitMetaData for current project
gitMetaData :: MonadIO m => m (Maybe GitMetaData)
gitMetaData = do
  head_ <- gitHead
  branch <- gitBranch
  remotes <- gitRemotes
  pure $ GitMetaData <$> head_ <*> branch <*> remotes

gitHead :: MonadIO m => m (Maybe GitHead)
gitHead = do
  id_ <- runGitLog "%H"
  author_name <- runGitLog "%aN"
  author_email <- runGitLog "%aE"
  committer_name <- runGitLog "%cN"
  committer_email <- runGitLog "%cE"
  message <- runGitLog "%s"
  pure $ GitHead <$> id_
    <*> author_name
    <*> author_email
    <*> committer_name
    <*> committer_email
    <*> message

gitBranch :: MonadIO m => m (Maybe Text)
gitBranch = runCommand "git rev-parse --abbrev-ref HEAD"

gitRemotes :: MonadIO m => m (Maybe [GitRemote])
gitRemotes = fmap (mapMaybe parseRemotes . T.lines) <$> runCommand "git remote -v"

parseRemotes :: Text -> Maybe GitRemote
parseRemotes remote = case T.words remote of
  (name:url:"(fetch)":_) -> Just $ GitRemote name url
  _                      -> Nothing

runGitLog :: MonadIO m => String -> m (Maybe Text)
runGitLog format = runCommand gitLogCommand
  where
    gitLogCommand = "git --no-pager log -1 --pretty=format:" <> format

runCommand :: MonadIO m => String -> m (Maybe Text)
runCommand cmd = do
  (exitCode, output) <- readProcessStdout $ shell cmd
  case exitCode of
    ExitSuccess   -> pure $ Just $ T.strip $ T.decodeUtf8 $ SB.toStrict output
    ExitFailure _ -> pure Nothing
