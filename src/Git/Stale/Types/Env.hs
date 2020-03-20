-- |
-- Module      : Git.Stale.Types.Env
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides `Env` type.
module Git.Stale.Types.Env
  ( BranchType (..),
    Env (..),
    branchTypeToArg,
  )
where

import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import qualified System.IO as IO
import Git.Stale.Types.Nat

-- | Describes the branch type.
data BranchType
  = All
  | Remote
  | Local
  deriving (Eq, Show)

-- | Maps a `BranchType` to a `String` flag to be used in "Core.FindBranches"'
-- `Core.FindBranches.branchNamesByGrep` command.
branchTypeToArg :: BranchType -> String
branchTypeToArg All = "-a"
branchTypeToArg Remote = "-r"
branchTypeToArg Local = ""

-- | The Env type to be used with Reader.
data Env
  = Env
      { -- | A `String` to filter branch names on. `Just` /s/ if non-empty,
        -- `Nothing` otherwise.
        grepStr :: Maybe T.Text,
        -- | The path of the git directory. `Just` /s/ if non-empty,
        -- `Nothing` otherwise.
        path :: Maybe IO.FilePath,
        -- | A non-negative integer descrbing stale threshold in days.
        limit :: Nat,
        -- | The type of branches to search.
        branchType :: BranchType,
        -- | The name of the remote.
        remoteName :: T.Text,
        -- | The name of the branch to consider merges against.
        master :: T.Text,
        -- | Today's date.
        today :: C.Day
      }
  deriving (Show)
