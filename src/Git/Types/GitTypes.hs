-- |
-- Module      : Git.Types.GitTypes
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides types to describe data returned by the
-- underlying `Core.FindBranches.FindBranches` monad.
module Git.Types.GitTypes
  ( Name (..),
    Author (..),
    NameLog,
    NameAuthDateStr,
    NameAuthDay,
  )
where

import qualified Data.Text as T
import qualified Data.Time.Calendar as C

-- | Branch Name.
newtype Name = Name T.Text deriving (Eq, Ord, Show)

-- | Branch Author.
newtype Author = Author T.Text deriving (Eq, Ord, Show)

-- | Intermediate tuple for (`Name`, log info).
type NameLog = (Name, T.Text)

-- | Intermediate tuple for (`Name`, `Author`, date string)
type NameAuthDateStr = (Name, Author, T.Text)

-- | Intermediate tuple for (`Name`, `Author`, `C.Day`)
type NameAuthDay = (Name, Author, C.Day)
