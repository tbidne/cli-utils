module Error (Err(..)) where

import Data.Text (Text)

data Err
  = ParseLog Text
  | ParseDate Text
  | ParseMerge Text
  | GitBranches Text
  | GitMerge Text
  | GitLog Text
  deriving Show