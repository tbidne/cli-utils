-- |
-- Module      : Git.FastForward.Types.Env
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides `Env` type.
module Git.FastForward.Types.Env
  ( Env (..),
  )
where

import Git.FastForward.Types.MergeType
import Git.Types.GitTypes

-- | The Env type to be used with Reader.
data Env
  = Env
      { -- | The path of the git directory. `Just` /s/ if non-empty,
        -- `Nothing` otherwise.
        path :: Maybe FilePath,
        -- | The type of merge to perform.
        mergeType :: MergeType,
        -- | Branches to push to remote.
        push :: [Name],
        -- | Determines if we perform @git fetch@ first.
        doFetch :: Bool
      }
  deriving (Show)
