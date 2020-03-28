module Git.Types.Arbitraries where

import Git.Types.GitTypes
import Test.QuickCheck
import qualified Data.Text as T

instance Arbitrary Name where
  arbitrary = do
    (PrintableString s) <- arbitrary
    pure $ Name $ T.pack s