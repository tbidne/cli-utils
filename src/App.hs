{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
  ( AppT (..),
  )
where

import qualified Control.Monad.Reader as R
import Types.Env

newtype AppT m a = AppT {runAppT :: R.ReaderT Env m a}
  deriving (Functor, Applicative, Monad, R.MonadIO, R.MonadTrans, R.MonadReader Env)
