module Types.Error
  ( Err(..)
  , ErrOr
  )
where

import           Data.Text                      ( Text )

data Err
  = ParseLog    Text
  | ParseDate   Text
  | ParseMerge  Text
  | ReadInt     Text
  | GitBranches Text
  | GitLog      Text
  deriving Show

type ErrOr a = Either Err a
