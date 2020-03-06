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

import qualified Data.Text as T 
import qualified Data.Time.Calendar as C

newtype Name = Name T.Text deriving (Eq, Ord, Show)
newtype Author = Author T.Text deriving (Eq, Ord, Show)
type NameLog = (Name, T.Text)
type NameAuthDateStr = (Name, Author, T.Text)
type NameAuthDay = (Name, Author, C.Day)

newtype Filtered a = Filtered { unFiltered :: [a] }
  deriving (Functor, Applicative, Monad)

mkFiltered :: (a -> Bool) -> [a] -> Filtered a
mkFiltered f = Filtered . filter f
