module Git.FastForward.Types.LocalBranches
  ( CurrentBranch,
    LocalBranches (..),
  )
where

import Git.Types.GitTypes

type CurrentBranch = Name

data LocalBranches
  = LocalBranches
      { current :: CurrentBranch,
        branches :: [Name]
      }
  deriving (Show)
