{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Colog.Reader (LoggingC (..), runLogging) where

import Colog.Core.Action
import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Colog
import Control.Monad.Trans.Class

newtype LoggingC msg m a = LoggingC {runLoggingC :: ReaderC (LogAction m msg) m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (LoggingC msg) where lift = LoggingC . lift

instance forall msg sig m. (Algebra sig m) => Algebra ((Log msg) :+: sig) (LoggingC msg m) where
  alg hdl sig ctx = do
    act <- LoggingC (asks @(LogAction m msg) unLogAction)
    case sig of
      L (Log s) -> lift (ctx <$ act s)
      R other -> LoggingC (alg (runLoggingC . hdl) (R other) ctx)
  {-# INLINE alg #-}

runLogging :: LogAction m msg -> LoggingC msg m a -> m a
runLogging l (LoggingC m) = runReader l m
