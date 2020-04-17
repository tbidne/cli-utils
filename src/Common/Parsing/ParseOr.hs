-- |
-- Module      : Common.Parsing.ParseOr
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module Common.Parsing.ParseOr
  ( ParseOr (..),
    module Common.Parsing.ParseStatus,
  )
where

import Common.Parsing.ParseStatus

-- | Induces an \"Or\" monoidal structure on 'ParseStatus'. That is,
--
-- @
--   ('PFailure' p1) <> ... <> ('PFailure' pn) = 'PFailure' (p1 <> ... <> pn)
--   ('PFailure' p1) <> ... <> ('PFailure' pj) <> ('PSuccess' pk) ... = 'PSuccess' pk
-- @
newtype ParseOr acc = ParseOr (ParseStatus acc)
  deriving (Eq, Show)

instance Semigroup (ParseOr acc) where
  (ParseOr (PFailure l)) <> (ParseOr (PFailure r)) =
    ParseOr $ PFailure $ l <> r
  (ParseOr (PFailure _)) <> r = r
  l <> _ = l

instance Monoid (ParseOr acc) where
  mempty = ParseOr $ PFailure $ Err ""
