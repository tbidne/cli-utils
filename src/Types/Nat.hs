module Types.Nat
  ( Nat,
    mkNat,
    unNat,
  )
where

-- | Represents a non-negative `Integer`.
newtype Nat = Nat {unNat :: Integer}
  deriving (Eq, Show)

-- | Constructs `Maybe` `Nat` as
--   \[
--     \mathrm{mkNat}(x) = \begin{cases}
--       \mathrm{Just}\ x, \quad &x >= 0 \\
--       \mathrm{Nothing}, \quad &\mathrm{otherwise}
--     \end{cases}
--   \]
mkNat :: Integer -> Maybe Nat
mkNat x
  | x >= 0 = Just $ Nat x
  | otherwise = Nothing
