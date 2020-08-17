{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Colog.Trace (LoggingC (..), runLogging) where

import Control.Algebra
import Control.Effect.Colog
import Control.Effect.Trace
import Control.Monad.Trans.Class

newtype LoggingC m a = LoggingC {runLoggingC :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans LoggingC where lift = LoggingC

instance forall sig m. (Has Trace sig m, Algebra sig m) => Algebra ((Log String) :+: sig) (LoggingC m) where
  alg hdl sig ctx = do
    case sig of
      L (Log s) -> LoggingC (ctx <$ trace s)
      R other -> LoggingC (alg (runLoggingC . hdl) other ctx)
  {-# INLINE alg #-}

runLogging :: LoggingC m a -> m a
runLogging = runLoggingC
{-# INLINE runLogging #-}
