{-# LANGUAGE OverloadedStrings #-}

module Git.FastForward.Core.Internal
  ( alreadyUpdated,
    textToLocalBranches,
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import Git.FastForward.Types.LocalBranches
import Git.Types.GitTypes

data LocalBranchesParser = LocalBranchesParser (Maybe CurrentBranch) [Name]

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

alreadyUpdated :: T.Text -> Bool
alreadyUpdated = T.isInfixOf "Already up-to-date"
