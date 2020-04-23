-- |
-- Module      : Common.Parsing.ParseAnd
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides the 'ParseAnd' monoid.
module Common.Parsing.ParseAnd
  ( ParseAnd (..),
    module Common.Parsing.ParseStatus,
  )
where

import Common.Parsing.ParseStatus

-- | 'ParseAnd' induces an \"And\" monoidal structure on @Monoid acc => 'ParseStatus' acc@.
-- That is,
--
-- @
--   ('PSuccess' p1) <> ... <> ('PSuccess' pn) = 'PSuccess' (p1 <> ... <> pn)
--   ('PSuccess' p1) <> ... <> ('PSuccess' pj) <> ('PFailure' pk) ... = 'PFailure' pk
-- @
--
-- The algebra for 'ParseAnd' satisfies
--
-- @
--   1. Identity: 'ParseAnd' ('PSuccess' mempty)
--   2. 'PFailure' is an ideal: 'PFailure' x <> 'PSuccess' y == 'PFailure' x == 'PSuccess' y <> 'PFailure' x
--   3. Left-biased: l <> r == l, when it doesn't violate 1 or 2.
-- @
--
-- Strictly speaking property 2 is stronger than saying 'PFailure' is an ideal.
-- More precisely, the action by 'PSuccess' on 'PFailure' in 'ParseAnd' is trivial.
newtype ParseAnd acc = ParseAnd (ParseStatus acc)
  deriving (Eq, Show)

instance Semigroup acc => Semigroup (ParseAnd acc) where
  (ParseAnd (PFailure l)) <> _ = ParseAnd $ PFailure l
  _ <> (ParseAnd (PFailure r)) = ParseAnd $ PFailure r
  (ParseAnd (PSuccess l)) <> (ParseAnd (PSuccess r)) =
    ParseAnd $ PSuccess $ l <> r

instance Monoid acc => Monoid (ParseAnd acc) where
  mempty = ParseAnd (PSuccess mempty)
