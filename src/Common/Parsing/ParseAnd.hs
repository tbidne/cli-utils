-- |
-- Module      : Common.Parsing.ParseAnd
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module Common.Parsing.ParseAnd
  ( ParseAnd (..),
    module Common.Parsing.ParseStatus,
  )
where

import Common.Parsing.ParseStatus

-- | Induces an \"And\" monoidal structure on 'ParseStatus'. That is,
--
-- @
--   ('PSuccess' p1) <> ... <> ('PSuccess' pn) = 'PSuccess' (p1 <> ... <> pn)
--   ('PSuccess' p1) <> ... <> ('PSuccess' pj) <> ('PFailure' pk) ... = 'PFailure' pk
-- @
newtype ParseAnd acc = ParseAnd (ParseStatus acc)
  deriving (Eq, Show)

instance Semigroup acc => Semigroup (ParseAnd acc) where
  (ParseAnd (PFailure l)) <> _ = ParseAnd $ PFailure l
  _ <> (ParseAnd (PFailure r)) = ParseAnd $ PFailure r
  (ParseAnd (PSuccess l)) <> (ParseAnd (PSuccess r)) =
    ParseAnd $ PSuccess $ l <> r

instance Monoid acc => Monoid (ParseAnd acc) where
  mempty = ParseAnd (PSuccess mempty)
