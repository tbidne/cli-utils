-- |
-- Module      : Common.Types.NonNegative
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module Common.Types.NonNegative
  ( NonNegative,
    iToNonNegative,
    toNonNegative,
    getNonNegative,
  )
where

-- | Represents a non-negative `Integer`.
newtype NonNegative = NonNegative {getNonNegative :: Integer}
  deriving (Eq, Show)

-- | Constructs `Maybe` `NonNegative` as
--   \[
--     \mathrm{toNonNegative}(x) = \begin{cases}
--       \mathrm{Just}\ x, &x >= 0 \\
--       \mathrm{Nothing}, &\mathrm{otherwise}
--     \end{cases}
--   \]
toNonNegative :: Integral a => a -> Maybe NonNegative
toNonNegative x
  | x >= 0 = Just $ NonNegative $ fromIntegral x
  | otherwise = Nothing

-- | Monomorphic version of 'toNonNegative' for convenience.
iToNonNegative :: Integer -> Maybe NonNegative
iToNonNegative = toNonNegative