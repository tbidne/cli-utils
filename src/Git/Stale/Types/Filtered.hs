-- |
-- Module      : Git.Stale.Types.Filtered
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
module Git.Stale.Types.Filtered
  ( Filtered,
    mkFiltered,
    unFiltered,
  )
where

-- | Type-safe way to guarantee a list has been filtered.
newtype Filtered a
  = Filtered
      { -- | Unwraps a 'Filtered'.
        unFiltered :: [a]
      }

instance Show a => Show (Filtered a) where
  show = show . unFiltered

-- | Constructs 'Filtered' based on filter function.
mkFiltered :: (a -> Bool) -> [a] -> Filtered a
mkFiltered f = Filtered . filter f
