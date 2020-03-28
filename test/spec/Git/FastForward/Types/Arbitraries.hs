module Git.FastForward.Types.Arbitraries where

import Git.FastForward.Types.UpdateResult
import Git.Types.GitTypes ()
import qualified Test.QuickCheck as QC
import Git.Types.Arbitraries ()

instance QC.Arbitrary UpdateResult where
  arbitrary = do
    name <- QC.arbitrary
    QC.elements [Failure name, NoChange name, Success name]
