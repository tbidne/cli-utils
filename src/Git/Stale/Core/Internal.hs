{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Git.Stale.Core.Internal
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports utility functions mainly for parsing/transforming
-- between types/errors.
module Git.Stale.Core.Internal
  ( badBranch,
    exceptToErr,
    parseLog,
    parseAuthDateStr,
    parseDay,
    safeRead,
    stale,
    staleNonErr,
    textToName,
    unsafeToInt,
  )
where

import Control.Monad ((>=>))
import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import Git.Stale.Types.Error
import Git.Stale.Types.Nat
import Git.Types.GitTypes
import qualified Text.Read as R

-- | Parses `NameLog` into `NameAuthDay`, recording errors
-- as `ErrOr`.
parseLog :: NameLog -> ErrOr NameAuthDay
parseLog = parseAuthDateStr >=> parseDay

-- | Intermediate parsing of `NameLog` into `NameAuthDateStr`,
-- recording errors as `ErrOr`.
parseAuthDateStr :: NameLog -> ErrOr NameAuthDateStr
parseAuthDateStr (n, l) = case T.splitOn "|" l of
  [a, t] -> Right (n, Author a, t)
  _ -> Left $ ParseLog l

-- | Intermediate parsing of `NameAuthDateStr` into `NameAuthDay`,
-- recording errors as `ErrOr`.
parseDay :: NameAuthDateStr -> ErrOr NameAuthDay
parseDay (n, a, t) = fmap (n,a,) eitherDay
  where
    eitherDay = case traverse safeRead (T.splitOn "-" t) of
      Right [y, m, d] -> Right $ C.fromGregorian (toInteger y) m d
      Right xs -> Left $ ParseDate $ T.pack $ show xs
      Left x -> Left x

-- | Determines if `NameAuthDay` is stale given by
--
-- > stale lim day (_, _, d) <=> day - d >= lim
stale :: Nat -> C.Day -> NameAuthDay -> Bool
stale lim day (_, _, d) = C.diffDays day d >= unNat lim

-- | For `Right` `NameAuthDay`, behaves the same as `stale`.
-- But for `Left` `Err` it is always true, since we do not want
-- to filter out errors.
staleNonErr :: Nat -> C.Day -> ErrOr NameAuthDay -> Bool
staleNonErr _ _ (Left _) = True
staleNonErr i d (Right nad) = stale i d nad

-- | Unsafely reads `T.Text` to `Int`.
unsafeToInt :: T.Text -> Int
unsafeToInt = read . T.unpack

-- | Safely reads `T.Text` into `ErrOr` `Int`.
safeRead :: T.Text -> ErrOr Int
safeRead t = case R.readMaybe (T.unpack t) of
  Nothing -> Left $ ReadInt t
  Just i -> Right i

-- | Joins nested `Either`s.
exceptToErr :: Show a => Either a (Either Err b) -> Either Err b
exceptToErr (Left x) = Left $ GitLog $ T.pack (show x)
exceptToErr (Right r) = r

-- | Tests for bad branches based on presence of /*/ and />/.
badBranch :: T.Text -> Bool
badBranch s = foldr f False ['*', '>']
  where
    f badChar b = b || T.any (== badChar) s

-- | Strips whitespace and potential git prefix (i.e. remotes/).
-- Works for strings with no prefix and those that have the prefix
-- exactly once, e.g.
--
-- > textToName "branch" -> branch
-- > textToName "remotes/origin/branch" -> origin/branch
-- > textToName "blah/remotes/origin/branch" -> origin/branch
--
-- Returns `Left` `ParseName` if the prefix occurs more than once as we
-- have definitely entered undefined territory.
textToName :: T.Text -> ErrOr Name
textToName b =
  case T.splitOn "remotes/" (T.strip b) of
    [_, nm] -> Right $ Name nm
    [nm] -> Right $ Name nm
    _ -> Left $ ParseName b
