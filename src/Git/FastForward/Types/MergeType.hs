module Git.FastForward.Types.MergeType
  ( MergeType (..),
  )
where

import Git.Types.GitTypes

data MergeType
  = Upstream
  | Master
  | Other Name
  deriving (Eq, Show)