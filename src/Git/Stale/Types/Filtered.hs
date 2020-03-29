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

-- | Intermediate type to ensure `Core.FindBranches.getStaleLogs`
-- filters stale logs.
newtype Filtered a = Filtered {unFiltered :: [a]}

instance Show a => Show (Filtered a) where
  show = show . unFiltered

instance Functor Filtered where
  fmap f (Filtered xs) = Filtered (fmap f xs)

instance Applicative Filtered where
  pure x = Filtered (pure x)
  (Filtered fs) <*> (Filtered xs) = Filtered (fs <*> xs)

instance Monad Filtered where
  (Filtered xs) >>= f = Filtered (xs >>= (unFiltered . f))

-- | Constructs `Filtered` 'a' based on filter function.
mkFiltered :: (a -> Bool) -> [a] -> Filtered a
mkFiltered f = Filtered . filter f
