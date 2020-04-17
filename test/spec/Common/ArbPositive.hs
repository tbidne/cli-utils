module Common.ArbPositive
  ( Q.Arbitrary (..),
  )
where

import Common.Types.Positive
import qualified Test.QuickCheck as Q

instance Q.Arbitrary Positive where
  arbitrary = do
    (Q.Positive p) <- Q.arbitrary :: Q.Gen (Q.Positive Integer)
    case toPositive p of
      Just p' -> pure p'
      Nothing -> error $ "Error creating Positive from: " <> show p
