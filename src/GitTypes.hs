module GitTypes
( Name(..)
, Log(..)
, Author(..)
, NameLog
, NameAuthDateStr
, NameAuthDay
, BranchLog(..)
) where

import Data.Text (Text)
import Data.Time.Calendar (Day)

newtype Name = Name Text deriving (Eq, Ord, Show)
newtype Log = Log Text deriving Show
newtype Author = Author Text deriving (Eq, Ord, Show)
type NameLog = (Name, Log)
type NameAuthDateStr = (Name, Author, Text)
type NameAuthDay = (Name, Author, Day)

newtype BranchLog = BranchLog (Name, Author, Day)
  deriving (Eq, Ord, Show)