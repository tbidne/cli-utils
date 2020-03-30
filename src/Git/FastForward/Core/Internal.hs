{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.FastForward.Core.Internal
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports utility functions.
module Git.FastForward.Core.Internal
  ( branchUpToDate,
    remoteUpToDate,
    textToLocalBranches,
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import Git.FastForward.Types.LocalBranches
import Git.Types.GitTypes

data LocalBranchesParser = LocalBranchesParser (Maybe CurrentBranch) [Name]

-- | Maps 'T.Text' to 'Just' 'LocalBranches' if we find a current branch
-- and all branch names are otherwise parsed successfully. Returns 'Nothing'
-- otherwise.
textToLocalBranches :: T.Text -> Maybe LocalBranches
textToLocalBranches s =
  let ls = T.lines s
      res = linesToParser ls
   in case res of
        LocalBranchesParser (Just curr) ns -> Just $ LocalBranches curr ns
        _ -> Nothing

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
branchUpToDate = (==) "Already up to date.\n"

-- | Determines if a remote branch is up to date.
remoteUpToDate :: T.Text -> Bool
remoteUpToDate = (==) "Everything up-to-date\n"
