{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.GitTypes
  ( Name(..)
  , Author(..)
  , NameLog
  , NameAuthDateStr
  , NameAuthDay
  , Filtered
  , mkFiltered
  , unFiltered
  )
where

import           Data.Text                      ( Text )
import           Data.Time.Calendar             ( Day )

newtype Name = Name Text deriving (Eq, Ord, Show)
newtype Author = Author Text deriving (Eq, Ord, Show)
type NameLog = (Name, Text)
type NameAuthDateStr = (Name, Author, Text)
type NameAuthDay = (Name, Author, Day)

newtype Filtered a = Filtered { unFiltered :: [a] }
  deriving (Functor, Applicative, Monad)

mkFiltered :: (a -> Bool) -> [a] -> Filtered a
mkFiltered f = Filtered . filter f
