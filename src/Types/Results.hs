{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Results
  ( Results(..)
  , toResults
  )
where

import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import           Types.Branch
import           Types.GitTypes

type BranchMap a = M.Map Author [Branch a]

data Results = Results
  { mergedMap :: BranchMap 'Merged
  , unmergedMap :: BranchMap 'UnMerged
  }

instance Show Results where
  show Results {..} = concat str
   where
    (m, s) = displayMap mergedMap
    (u, t) = displayMap unmergedMap
    str =
      [ "MERGED: "
      , show s
      , "\n------\n"
      , m
      , "\n\n"
      , "UNMERGED: "
      , show t
      , "\n--------\n"
      , u
      ]

displayMap :: M.Map Author [Branch a] -> (String, Int)
displayMap = M.foldrWithKey f ("", 0)
 where
  f :: Author -> [Branch a] -> (String, Int) -> (String, Int)
  f (Author k) ns acc = case ns of
    [] -> acc
    xs ->
      ( T.unpack k
        <> " ("
        <> show (length xs)
        <> "): "
        <> show ns
        <> "\n\n"
        <> fst acc
      , snd acc + length xs
      )

toResults :: [AnyBranch] -> Results
toResults = F.foldl' go mkResults
 where
  go mp (MergedBranch   b@(MkBranch _ a _)) = insertMerged a b mp
  go mp (UnMergedBranch b@(MkBranch _ a _)) = insertUnMerged a b mp

mkResults :: Results
mkResults = Results M.empty M.empty

insertMerged :: Author -> Branch 'Merged -> Results -> Results
insertMerged a b Results {..} = Results mergedMap' unmergedMap
 where
  mergedMap' = case M.lookup a mergedMap of
    Nothing -> M.insert a [b] mergedMap
    Just bs -> M.insert a (b : bs) mergedMap

insertUnMerged :: Author -> Branch 'UnMerged -> Results -> Results
insertUnMerged a b Results {..} = Results mergedMap unmergedMap'
 where
  unmergedMap' = case M.lookup a unmergedMap of
    Nothing -> M.insert a [b] unmergedMap
    Just bs -> M.insert a (b : bs) unmergedMap
