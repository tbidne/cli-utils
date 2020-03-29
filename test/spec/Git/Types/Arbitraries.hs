module Git.Types.Arbitraries where

import qualified Data.Text as T
import Git.Types.GitTypes
import Test.QuickCheck

instance Arbitrary Name where
  arbitrary = do
    (PrintableString s) <- arbitrary
    pure $ Name $ T.pack s
