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
    MergedDisp (..),
    UnMergedDisp (..),
    ResultsDisp (..),
    toResultsDisp,
    toResults,
  )
where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Git.Stale.Types.Branch
import Git.Types.GitTypes

-- | Wrapper for holding @`M.Map` `Author` [`Branch` a]@ data.
data Results
  = Results
      { -- | Holds the results for merged branches
        mergedMap :: M.Map Author [Branch 'Merged],
        -- | Holds the results for unmerged branches
        unMergedMap :: M.Map Author [Branch 'UnMerged]
      }
  deriving (Show)

-- | Newtype wrapper for merged branches over (display string, num branches).
newtype MergedDisp = MergedDisp (T.Text, Int)

-- | Newtype wrapper for unmerged branches over (display string, num branches).
newtype UnMergedDisp = UnMergedDisp (T.Text, Int)

-- | Newtype wrapper over ('MergedDisp', 'UnMergedDisp').
newtype ResultsDisp = ResultsDisp (MergedDisp, UnMergedDisp)

-- | Transforms the 'Results' into a 'ResultsDisp' for display purposes.
-- Strips out the @prefix@ from the branches, if it exists.
toResultsDisp :: T.Text -> Results -> ResultsDisp
toResultsDisp prefix Results {mergedMap, unMergedMap} = ResultsDisp (merged, unmerged)
  where
    merged = MergedDisp $ displayMap prefix mergedMap
    unmerged = UnMergedDisp $ displayMap prefix unMergedMap

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
