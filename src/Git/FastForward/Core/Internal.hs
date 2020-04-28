{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.FastForward.Core.Internal
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports utility functions.
module Git.FastForward.Core.Internal
  ( branchUpToDate,
    mergeTypeToCmd,
    remoteUpToDate,
    textToLocalBranches,
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import Git.FastForward.Types.LocalBranches
import Git.FastForward.Types.MergeType
import Git.Types.GitTypes

data LocalBranchesParser = LocalBranchesParser (Maybe CurrentBranch) [Name]

-- | Maps 'T.Text' to 'Right' 'LocalBranches' if we find a current branch
-- and all branch names are otherwise parsed successfully. Returns 'Left' err
-- otherwise.
textToLocalBranches :: T.Text -> Either T.Text LocalBranches
textToLocalBranches s =
  let ls = T.lines s
      res = linesToParser ls
   in case res of
        LocalBranchesParser (Just curr) ns -> Right $ LocalBranches curr ns
        _ -> Left $ "Error parsing local branches: " <> s

linesToParser :: [T.Text] -> LocalBranchesParser
linesToParser = F.foldl' f (LocalBranchesParser Nothing [])
  where
    f (LocalBranchesParser curr ns) txt =
      case starredBranch txt of
        Right curr' -> LocalBranchesParser (Just curr') (curr' : ns)
        Left other -> LocalBranchesParser curr (other : ns)

starredBranch :: T.Text -> Either Name CurrentBranch
starredBranch b =
  case T.splitOn "* " b of
    [_, curr] -> Right $ Name $ T.strip curr
    _ -> Left $ Name $ T.strip b

-- | Determines if a local branch is up to date.
branchUpToDate :: T.Text -> Bool
branchUpToDate t =
  T.isPrefixOf "Already up to date" t
    || T.isPrefixOf "Already up-to-date" t

-- | Determines if a remote branch is up to date.
remoteUpToDate :: T.Text -> Bool
remoteUpToDate t =
  T.isPrefixOf "Everything up-to-date" t
    || T.isPrefixOf "Everything up to date" t

-- | Maps a merge type to its command
mergeTypeToCmd :: MergeType -> T.Text
mergeTypeToCmd Upstream = "git merge @{u} --ff-only"
mergeTypeToCmd Master = "git merge origin/master --ff-only"
mergeTypeToCmd (Other (Name up)) = "git merge \"" <> up <> "\" --ff-only"
