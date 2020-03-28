module Git.FastForward.Types.Env
  ( Env (..),
  )
where

import Git.FastForward.Types.MergeType

data Env
  = Env
      { -- | The path of the git directory. `Just` /s/ if non-empty,
        -- `Nothing` otherwise.
        path :: Maybe FilePath,
        -- | The type of merge to perform.
        mergeType :: MergeType
      }
  deriving (Show)