-- |
-- Module      : Git.FastForward.Types.LocalBranches
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides `LocalBranches` type.
module Git.FastForward.Types.LocalBranches
  ( CurrentBranch,
    LocalBranches (..),
  )
where

import Git.Types.GitTypes

-- | Alias for 'Name'.
type CurrentBranch = Name

-- | Describes the local branches in a git directory.
data LocalBranches
  = LocalBranches
      { -- | The current branch, saved so that we can check it out after
        -- we are done updating all branches.
        current :: CurrentBranch,
        -- | List of all branches.
        branches :: [Name]
      }
  deriving (Show)
