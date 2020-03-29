{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Output where

import qualified Data.Text as T

data Output a = Output [T.Text] a
  deriving (Eq, Ord, Show, Functor)

instance Semigroup a => Semigroup (Output a) where
  (Output rs x) <> (Output ts y) = Output (rs <> ts) (x <> y)

instance Monoid a => Monoid (Output a) where
  mempty = Output [] mempty

instance Applicative Output where
  pure :: a -> Output a
  pure = Output []

  (<*>) :: Output (a -> b) -> Output a -> Output b
  (Output rs f) <*> (Output ts x) = Output (rs <> ts) (f x)

instance Monad Output where
  (>>=) :: Output a -> (a -> Output b) -> Output b
  (Output rs x) >>= f = Output (rs <> ts) y where (Output ts y) = f x

putOutput :: Show a => [a] -> Output ()
putOutput xs = Output (fmap (T.pack . show) xs) ()

putSingleOutput :: Show a => a -> Output ()
putSingleOutput x = Output [(T.pack . show) x] ()

prependOut :: [T.Text] -> Output a -> Output a
prependOut ts (Output rs x) = Output (ts <> rs) x
