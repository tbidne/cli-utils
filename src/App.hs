{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : App
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Provides the main component used in this application.
module App
  ( AppT (..),
    mapAppT,
  )
where

import qualified Control.Monad.Logger as L
import qualified Control.Monad.Reader as R

newtype AppT e m a = AppT {runAppT :: R.ReaderT e (L.LoggingT m) a}

instance Functor m => Functor (AppT e m) where
  fmap f (AppT m) = AppT (fmap f m)

instance Applicative f => Applicative (AppT e f) where
  pure = AppT . pure
  (AppT f) <*> (AppT m) = AppT (f <*> m)

instance Monad m => Monad (AppT e m) where
  (AppT m) >>= f = AppT (m >>= (runAppT . f))

instance R.MonadTrans (AppT e) where
  lift = AppT . R.lift . R.lift

instance R.MonadIO m => R.MonadIO (AppT e m) where
  liftIO = R.lift . R.liftIO

instance Monad m => R.MonadReader e (AppT e m) where
  ask = AppT R.ask
  local = R.local
  reader = R.reader

instance R.MonadIO m => L.MonadLogger (AppT e m) where
  monadLoggerLog a b c d = AppT $ R.lift $ L.monadLoggerLog a b c d

mapAppT ::
  (R.ReaderT e (L.LoggingT m) a -> R.ReaderT e (L.LoggingT n) b) ->
  AppT e m a ->
  AppT e n b
mapAppT f = AppT . f . runAppT
