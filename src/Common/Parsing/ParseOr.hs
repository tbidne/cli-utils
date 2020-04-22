-- |
-- Module      : Common.Parsing.ParseOr
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides the 'ParseOr' monoid.
module Common.Parsing.ParseOr
  ( ParseOr (..),
    module Common.Parsing.ParseStatus,
  )
where

import Common.Parsing.ParseStatus

-- | 'ParseOr' Induces an \"Or\" monoidal structure on 'ParseStatus'. That is,
--
-- @
--   ('PFailure' p1) <> ... <> ('PFailure' pn) = 'PFailure' (p1 <> ... <> pn)
--   ('PFailure' p1) <> ... <> ('PFailure' pj) <> ('PSuccess' pk) ... = 'PSuccess' pk
-- @
--
-- The algebra for 'ParseOr' satisfies
--
-- @
--   1. Identity: 'ParseOr' ('PFailure' ('Err' ""))
--   2. 'PSuccess' is an ideal: 'PSuccess' x <> 'PFailure' y == 'PSuccess' x == 'PFailure' y <> 'PSuccess' x
--   3. Left-biased: l <> r == l, when it doesn't violate 1 or 2.
-- @
--
-- Strictly speaking property 2 is stronger than saying 'PSuccess' is an ideal.
-- More precisely, the action by 'PFailure' on 'PSuccess' in 'ParseOr' is trivial.
newtype ParseOr acc = ParseOr (ParseStatus acc)
  deriving (Eq, Show)

instance Semigroup (ParseOr acc) where
  (ParseOr (PFailure l)) <> (ParseOr (PFailure r)) =
    ParseOr $ PFailure $ l <> r
  (ParseOr (PFailure _)) <> r = r
  l <> _ = l

instance Monoid (ParseOr acc) where
  mempty = ParseOr $ PFailure $ Err ""
