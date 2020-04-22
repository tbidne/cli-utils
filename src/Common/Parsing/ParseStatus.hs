-- |
-- Module      : Common.Parsing.ParseStatus
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module Common.Parsing.ParseStatus
  ( ParseStatus (..),
    module Common.Parsing.ParseErr,
  )
where

import Common.Parsing.ParseErr

-- | Describes the outcome of a parse attempt into an accumulator @acc@.
data ParseStatus acc
  = PFailure ParseErr
  | PSuccess acc
  deriving (Eq, Show)
