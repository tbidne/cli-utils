{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.Stale.Types.Branch
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides types for holding branch information.
module Git.Stale.Types.Branch
  ( AnyBranch (..),
    BranchStatus (..),
    Branch (..),
    branchesToName,
    mkAnyBranch,
  )
where

import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import Git.Stale.Types.GitTypes

-- | Promoted data type used for adding type-safe merge status
-- to branches.
data BranchStatus where
  Merged :: BranchStatus
  UnMerged :: BranchStatus

-- | Holds branch information with phantom `BranchStatus`.
data Branch (a :: BranchStatus) where
  MkBranch :: Name -> Author -> C.Day -> Branch a
  deriving (Show)

-- | Maps each [`Branch`] to its `T.Text` `Name` and concatenates results.
-- Attempts to strip out an irrelevant prefix it may have (i.e. `Types.Env.remoteName`).
branchesToName :: T.Text -> [Branch a] -> T.Text
branchesToName prefix branches = "[" <> T.intercalate "," names <> "]"
  where
    names = branchToName prefix <$> branches

branchToName :: T.Text -> Branch a -> T.Text
branchToName prefix (MkBranch (Name n) _ _) =
  case prefix of
    "" -> n
    p ->
      case T.splitOn p (T.strip n) of
        [_, nm] -> nm
        [nm] -> nm
        _ -> "Err splitting " <> n <> " on " <> p

-- | Hides `BranchStatus` type parameter, used for processing
-- `Branch` a in heterogeneous collections.
data AnyBranch where
  MergedBranch :: Branch 'Merged -> AnyBranch
  UnMergedBranch :: Branch 'UnMerged -> AnyBranch
  deriving (Show)

-- | Constructs an `AnyBranch` wrapper around a `Branch` a
-- where a is `Merged` if passed `True`, `UnMerged` otherwise.
mkAnyBranch :: Name -> Author -> C.Day -> Bool -> AnyBranch
mkAnyBranch n a d b
  | b = MergedBranch $ MkBranch n a d
  | otherwise = UnMergedBranch $ MkBranch n a d
