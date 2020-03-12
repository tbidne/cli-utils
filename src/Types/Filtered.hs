{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Filtered
  ( Filtered,
    mkFiltered,
    unFiltered,
  )
where

-- | Intermediate type to ensure `Core.MonadStaleBranches.getStaleLogs`
-- filters stale logs.
newtype Filtered a = Filtered {unFiltered :: [a]}
  deriving (Show, Functor, Applicative, Monad)

-- | Constructs `Filtered` /a/ based on filter function.
mkFiltered :: (a -> Bool) -> [a] -> Filtered a
mkFiltered f = Filtered . filter f
