module Common.ArbNonNegative
  ( Q.Arbitrary (..),
  )
where

import Common.Types.NonNegative
import qualified Test.QuickCheck as Q

instance Q.Arbitrary NonNegative where
  arbitrary = do
    (Q.NonNegative lim) <- Q.arbitrary :: Q.Gen (Q.NonNegative Integer)
    case toNonNegative lim of
      Nothing -> error $ "Error creating NonNegative from: " <> show lim
      Just n -> pure n
