module Types.Error
  ( Err(..)
  , ErrOr
  )
where

import qualified Data.Text as T

data Err
  = ParseLog    T.Text
  | ParseDate   T.Text
  | ParseMerge  T.Text
  | ReadInt     T.Text
  | GitBranches T.Text
  | GitLog      T.Text
  deriving Show

type ErrOr a = Either Err a
