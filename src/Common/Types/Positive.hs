-- |
-- Module      : Common.Types.Positive
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module Common.Types.Positive
  ( Positive,
    iToPositive,
    toPositive,
    getPositive,
  )
where

-- | Represents a positive, non-zero `Integer`.
newtype Positive a
  = Positive
      { -- | Unwraps a 'Positive'.
        getPositive :: a
      }
  deriving (Eq, Ord, Show)

-- | Constructs `Maybe` `Positive` as
--   \[
--     \mathrm{toPositive}(x) = \begin{cases}
--       \mathrm{Just}\ x, &x > 0 \\
--       \mathrm{Nothing}, &\mathrm{otherwise}
--     \end{cases}
--   \]
toPositive :: Integral a => a -> Maybe (Positive a)
toPositive x
  | x > 0 = Just $ Positive x
  | otherwise = Nothing

-- | Monomorphic version of 'toPositive' for convenience.
iToPositive :: Integer -> Maybe (Positive Integer)
iToPositive = toPositive
