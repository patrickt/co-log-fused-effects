{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Control.Effect.Colog (Log (..), log, Has, send) where

import Prelude ()
import Control.Algebra
import Data.Kind (Type)

data Log msg (m :: Type -> Type) k where
  Log :: msg -> Log msg m ()

log :: Has (Log msg) sig m => msg -> m ()
log m = send (Log m)
{-# INLINE log #-}
