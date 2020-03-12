-- |
-- Module      : Types.Env
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides `Err` type for describing any errors encountered.
module Types.Error
  ( Err (..),
    ErrOr,
  )
where

import qualified Data.Text as T

-- | Wraps `T.Text` to describe an error. Git* errors describe errors
-- encountered by the underlying `Core.MonadStaleBranches.MonadStaleBranches`
-- monad. Others describe pure errors encountered during parsing data returned
-- by the monad.
data Err
  = ParseLog T.Text
  | ParseDate T.Text
  | ParseName T.Text
  | ParseMerge T.Text
  | ReadInt T.Text
  | GitBranches T.Text
  | GitLog T.Text
  deriving (Show)

-- | Alias for convenience.
type ErrOr a = Either Err a
