{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Git.Stale.Types.Results
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides a `Results` type that wraps two maps, one for merged branches
-- and one for unmerged.
module Git.Stale.Types.Results
  ( Results (..),
    displayResults,
    toResults,
  )
where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Git.Stale.Types.Branch
import Git.Stale.Types.GitTypes

type BranchMap a = M.Map Author [Branch a]

-- | Wrapper for holding `M.Map` `Author` [`Branch` a] data.
data Results
  = Results
      { -- | `M.Map` `Author` [`Branch` `Merged`]
        mergedMap :: BranchMap 'Merged,
        -- | `M.Map` `Author` [`Branch` `UnMerged`]
        unMergedMap :: BranchMap 'UnMerged
      }
  deriving (Show)

-- | Displays `Results`. Differs from `Show` in that it is formatted differently
-- and strips the `T.Text` /prefix/ from the branch names.
displayResults :: T.Text -> Results -> T.Text
displayResults prefix Results {mergedMap, unMergedMap} = T.concat str
  where
    (m, s) = displayMap prefix mergedMap
    (u, t) = displayMap prefix unMergedMap
    str =
      [ "MERGED: ",
        T.pack (show s),
        "\n------\n",
        m,
        "\n\n",
        "UNMERGED: ",
        T.pack (show t),
        "\n--------\n",
        u
      ]

displayMap :: T.Text -> M.Map Author [Branch a] -> (T.Text, Int)
displayMap prefix = M.foldrWithKey f ("", 0)
  where
    f :: Author -> [Branch a] -> (T.Text, Int) -> (T.Text, Int)
    f (Author k) ns acc = case ns of
      [] -> acc
      xs ->
        ( k
            <> " ("
            <> T.pack (show (length xs))
            <> "): "
            <> branchesToName prefix ns
            <> "\n\n"
            <> fst acc,
          snd acc + length xs
        )

-- | Maps [`AnyBranch`] to `Results`. Pattern matches on `AnyBranch` to reveal
-- the underlying `BranchStatus`, ensuring merged and unmerged branches are
-- organized separately.
toResults :: [AnyBranch] -> Results
toResults = F.foldl' go mkResults
  where
    go mp (MergedBranch b@(MkBranch _ a _)) = insertMerged a b mp
    go mp (UnMergedBranch b@(MkBranch _ a _)) = insertUnMerged a b mp

mkResults :: Results
mkResults = Results M.empty M.empty

insertMerged :: Author -> Branch 'Merged -> Results -> Results
insertMerged a b Results {mergedMap, unMergedMap} = Results mergedMap' unMergedMap
  where
    mergedMap' = case M.lookup a mergedMap of
      Nothing -> M.insert a [b] mergedMap
      Just bs -> M.insert a (b : bs) mergedMap

insertUnMerged :: Author -> Branch 'UnMerged -> Results -> Results
insertUnMerged a b Results {mergedMap, unMergedMap} = Results mergedMap unMergedMap'
  where
    unMergedMap' = case M.lookup a unMergedMap of
      Nothing -> M.insert a [b] unMergedMap
      Just bs -> M.insert a (b : bs) unMergedMap
