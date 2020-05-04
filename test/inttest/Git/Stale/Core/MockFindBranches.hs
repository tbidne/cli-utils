{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Git.Stale.Core.MockFindBranches
  ( MonadFindBranches (..),
  )
where

import Common.RefinedUtils
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import Git.Stale.Core.MonadFindBranches
import Git.Stale.Types.Branch
import Git.Stale.Types.Env
import Git.Stale.Types.Filtered
import Git.Types.GitTypes
import Output

instance MonadFindBranches Output where
  type Handler Output a = a
  type FinalResults Output = [AnyBranch]

  branchNamesByGrep ::
    Maybe FilePath ->
    BranchType ->
    Maybe T.Text ->
    Output [Name]
  branchNamesByGrep _ _ g = do
    let maybeFilter = case g of
          Nothing -> id
          Just s -> filter (\(Name n) -> s `T.isInfixOf` n)
    pure $ maybeFilter allBranches

  getStaleLogs ::
    Maybe FilePath ->
    RNonNegative Int ->
    Cal.Day ->
    [Name] ->
    Output (Filtered NameAuthDay)
  getStaleLogs _ _ _ ns = do
    let removeStale ((Name n), _, _) = not $ "stale" `T.isInfixOf` n
        toLog nm@(Name n) = (nm, Author n, error "MockFindBranches -> getStaleLogs: day not defined")
    pure $ (mkFiltered removeStale . fmap toLog) ns

  toBranches ::
    Maybe FilePath ->
    T.Text ->
    Filtered NameAuthDay ->
    Output [AnyBranch]
  toBranches _ _ = pure . fmap toBranch . unFiltered
    where
      toBranch (n, a, d) = mkAnyBranch n a d True

  collectResults :: [AnyBranch] -> Output [AnyBranch]
  collectResults = return

  display :: T.Text -> [AnyBranch] -> Output ()
  display _ = putShowList

allBranches :: [Name]
allBranches =
  [ Name "branch 1 stale",
    Name "branch 2 stale",
    Name "branch 3",
    Name "branch 4 stale",
    Name "branch 5",
    Name "branch 6",
    Name "branch 7",
    Name "branch 8",
    Name "other 1",
    Name "other 2 stale",
    Name "other 3",
    Name "other 4 stale",
    Name "other 5"
  ]
