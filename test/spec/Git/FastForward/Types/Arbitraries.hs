module Git.FastForward.Types.Arbitraries
  ( QC.Arbitrary (..),
  )
where

import Git.FastForward.Types.UpdateResult
import Git.Types.Arbitraries ()
import Git.Types.GitTypes ()
import qualified Test.QuickCheck as QC

instance QC.Arbitrary UpdateResult where
  arbitrary = do
    name <- QC.arbitrary
    QC.elements [Failure name, NoChange name, Success name]
