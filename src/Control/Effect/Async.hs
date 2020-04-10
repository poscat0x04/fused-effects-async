{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Effect.Async where

import Control.Algebra
import Control.Concurrent.Async

data Asynchronous m k where
  Async :: m a -> (Async a -> m k) -> Asynchronous m k
  Wait :: Async a -> (a -> m k) -> Asynchronous m k

async :: (Has Asynchronous sig m) => m a -> m (Async a)
async m = send (Async m pure)

wait :: (Has Asynchronous sig m) => Async a -> m a
wait a = send (Wait a pure)

instance Functor m => Functor (Asynchronous m) where
  fmap :: (k -> k') -> Asynchronous m k -> Asynchronous m k'
  fmap f (Async m c) = Async m (fmap f . c)
  fmap f (Wait a c) = Wait a (fmap f . c)

instance HFunctor Asynchronous where
  hmap :: Functor m => (forall x. m x -> n x) -> Asynchronous m a -> Asynchronous n a
  hmap f (Async m c) = Async (f m) (f . c)
  hmap f (Wait a c) = Wait a (f . c)
