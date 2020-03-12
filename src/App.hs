{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : App
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides the main component used in this application.
module App
  ( AppT (..),
  )
where

import qualified Control.Monad.Reader as R
import Types.Env

newtype AppT m a = AppT {runAppT :: R.ReaderT Env m a}

instance Functor m => Functor (AppT m) where
  fmap f (AppT m) = AppT (fmap f m)

instance Applicative f => Applicative (AppT f) where
  pure = AppT . pure
  (AppT f) <*> (AppT m) = AppT (f <*> m)

instance Monad m => Monad (AppT m) where
  (AppT m) >>= f = AppT (m >>= (runAppT . f))

instance R.MonadTrans AppT where
  lift = AppT . R.lift

instance R.MonadIO m => R.MonadIO (AppT m) where
  liftIO = R.lift . R.liftIO

instance Monad m => R.MonadReader Env (AppT m) where
  ask = AppT R.ask
  local = R.local
  reader = R.reader
