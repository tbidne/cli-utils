{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.ArbNonNegative
  ( Q.Arbitrary (..),
  )
where

import Common.Types.NonNegative
import qualified Test.QuickCheck as Q

instance (Integral a, Q.Arbitrary a, Show a) => Q.Arbitrary (NonNegative a) where
  arbitrary = do
    (Q.NonNegative lim) <- Q.arbitrary :: Q.Gen (Q.NonNegative a)
    case toNonNegative lim of
      Nothing -> error $ "Error creating NonNegative from: " <> show lim
      Just n -> pure n
