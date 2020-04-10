{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Async where

import Control.Algebra
import Control.Carrier.Reader
import qualified Control.Concurrent.Async as CA
import Control.Effect.Async
import Control.Effect.Lift
import Control.Monad.IO.Class

-- | All other effects will need to be handled first
newtype AsyncC m a = AsyncC ((forall x. m x -> IO x) -> m a)
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderC (forall x. m x -> IO x) m)

runAsyncC :: (forall x. m x -> IO x) -> AsyncC m a -> m a
runAsyncC hoist (AsyncC f) = f hoist

instance
  ( Algebra sig m,
    Has (Lift IO) sig m
  ) =>
  Algebra (Asynchronous :+: sig) (AsyncC m)
  where
  alg :: (Asynchronous :+: sig) (AsyncC m) a -> AsyncC m a
  alg (R other) = AsyncC $ \hoist -> alg $ hmap (runAsyncC hoist) other
  alg (L (Async (AsyncC m) c)) = (AsyncC $ \hoist -> sendIO $ CA.async $ hoist $ m hoist) >>= c
  alg (L (Wait a c)) = (AsyncC $ \hoist -> sendIO $ CA.wait a) >>= c
