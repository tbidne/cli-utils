module Types.Error (Err(..)) where

import Data.Text (Text)

data Err
  = ParseLog    Text
  | ParseDate   Text
  | ParseMerge  Text
  | ReadInt     Text
  | GitBranches Text
  | GitLog      Text
  deriving Show