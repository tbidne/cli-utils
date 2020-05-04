-- |
-- Module      : Common.RefinedUtils
-- License     : BSD3
-- Maintainer  : tbidne@gmail.com
-- Exports utility functions for working with 'R.Refined'.
module Common.RefinedUtils
  ( RNonNegative,
    RPositive,
    unsafeNonNeg,
    unsafePos,
    unsafeRef,
    module Refined,
  )
where

import qualified Control.Exception as Ex
import qualified Data.Either as E
import Refined

type RNonNegative a = Refined NonNegative a

type RPositive a = Refined Positive a

-- | 'unsafeRef' for 'R.NonNegative'.
unsafeNonNeg :: (Num x, Ord x) => x -> Refined NonNegative x
unsafeNonNeg = unsafeRef

-- | 'unsafeRef' for 'R.Positive'.
unsafePos :: (Num x, Ord x) => x -> Refined Positive x
unsafePos = unsafeRef

-- | Unsafe version of 'R.refine' that uses 'Ex.throw' when @x@ does not
-- satisfy the predicate @p@.
unsafeRef :: Predicate p x => x -> Refined p x
unsafeRef = E.either Ex.throw id . refine
