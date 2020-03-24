{-# LANGUAGE TypeFamilies #-}

module Git.Types.Handler
  ( Handler,
  )
where

import Data.Kind (Type)

-- | Used for adding custom handling to monadic actions (e.g. for error handling).
type family Handler (m :: Type -> Type) (a :: Type)
