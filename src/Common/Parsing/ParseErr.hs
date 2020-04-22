-- |
-- Module      : Common.Parsing.ParseErr
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
--
-- Provides the 'ParseErr' monoid.
module Common.Parsing.ParseErr
  ( ParseErr (..),
  )
where

-- | 'ParseErr' describes an error encountered while parsing.
-- Errors are either a request for 'Help' or some general error.
--
-- The algebra for 'ParseErr' satisfies
--
-- @
--   1. Identity: 'Err' ""
--   2. 'Help' is an ideal: 'Help' x <> 'Err' y == 'Help' x == 'Err' y <> 'Help' x
--   3. Left-biased: l <> r == l, when it doesn't violate 1 or 2.
-- @
--
-- Strictly speaking property 2 is stronger than saying 'Help' is an ideal.
-- More precisely, the action by 'Err' on 'Help' in 'ParseErr' is trivial.
data ParseErr
  = Err String
  | Help String
  deriving (Eq, Show)

instance Semigroup ParseErr where
  (Err "") <> r = r
  (Help l) <> _ = Help l
  (Err _) <> (Help r) = Help r
  (Err l) <> _ = Err l

instance Monoid ParseErr where
  mempty = Err ""
