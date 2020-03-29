-- |
-- Module      : Git.FastForward.Types.MergeType
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides `MergeType`.
module Git.FastForward.Types.MergeType
  ( MergeType (..),
  )
where

import Git.Types.GitTypes

-- | Describes the type of merge we're going to perform.
data MergeType
  = -- | Merges upstream via @{u}.
    Upstream
  | -- | Merges origin/master.
    Master
  | -- | Merges 'Name'.
    Other Name
  deriving (Eq, Show)
