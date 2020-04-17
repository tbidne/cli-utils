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
newtype Positive = Positive {getPositive :: Integer}
  deriving (Eq, Show)

-- | Constructs `Maybe` `Positive` as
--   \[
--     \mathrm{toPositive}(x) = \begin{cases}
--       \mathrm{Just}\ x, &x > 0 \\
--       \mathrm{Nothing}, &\mathrm{otherwise}
--     \end{cases}
--   \]
toPositive :: Integral a => a -> Maybe Positive
toPositive x
  | x > 0 = Just $ Positive $ fromIntegral x
  | otherwise = Nothing

-- | Monomorphic version of 'toPositive' for convenience.
iToPositive :: Integer -> Maybe Positive
iToPositive = toPositive