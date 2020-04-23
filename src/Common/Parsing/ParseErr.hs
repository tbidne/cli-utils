-- |
-- Module      : Common.Parsing.ParseErr
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module Common.Parsing.ParseErr
  ( ParseErr (..),
  )
where

-- | Describes anything other than a successful parse.
data ParseErr
  = -- | Indicates that there was an error when trying to parse 'String'.
    Err String
  | -- | Indicates the argument "--help" was passed.
    Help String
  deriving (Eq, Show)

-- | The 'Semigroup' instance for 'ParseErr' satisfies
--
-- @
--   1. Identity: 'Err' ""
--   2. Ideal 'Help': 'Help' x <> 'Err' 'y' == 'Help' 'x' == 'Err' 'y' <> 'Help' 'x'
--   3. Left-biased: l <> r == l, when it doesn't violate 1 or 2.
-- @
instance Semigroup ParseErr where
  (Err "") <> r = r
  (Help l) <> _ = Help l
  (Err _) <> (Help r) = Help r
  (Err l) <> _ = Err l

instance Monoid ParseErr where
  mempty = Err ""
