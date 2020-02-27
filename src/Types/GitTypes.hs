module Types.GitTypes
( Name(..)
, Author(..)
, NameLog
, NameAuthDateStr
, NameAuthDay
) where

import Data.Text (Text)
import Data.Time.Calendar (Day)

newtype Name = Name Text deriving (Eq, Ord, Show)
newtype Author = Author Text deriving (Eq, Ord, Show)
type NameLog = (Name, Text)
type NameAuthDateStr = (Name, Author, Text)
type NameAuthDay = (Name, Author, Day)