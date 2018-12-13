{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}

module Reactive.Types where

import qualified Data.Machine as M
import Control.Arrow
import qualified Control.Category as C
import Data.Functor.Identity
import Data.Void

newtype StreamT m a b = StreamT {toProcess :: M.ProcessT m a b}
  deriving (Functor)

deriving instance (Monad m, M.Appliance (M.Is a)) => Applicative (StreamT m a)
deriving instance (Monad m, M.Appliance (M.Is a), Monad (M.MachineT m (M.Is a))) => Monad (StreamT m a)

type Stream a b = forall m. StreamT m a b
type SourceT m b = StreamT m Void b
type SinkT m a = StreamT m a Void
type System m = StreamT m Void Void

instance (Monad m) => C.Category (StreamT m) where
  id = StreamT M.echo
  StreamT a . StreamT b  = StreamT (a M.<~ b)

instance (Monad m) => Arrow (StreamT m) where
  arr f = StreamT $ M.autoT @Kleisli (arr f)
  first (StreamT m') = StreamT (pairProcess m')
    where
      pairProcess m = M.MachineT $ do
        (output, await, _) <- yieldUntilAwait m
        return (M.Await (\(b, d) -> yieldFirst ((,d) <$> output) $ next d (await b)) M.Refl M.stopped)
      next d m = M.MachineT $ do
        (output, await, _) <- yieldUntilAwait m
        M.runMachineT $ yieldFirst ((,d) <$> output) (pairProcess . M.encased $ M.Await await M.Refl M.stopped)
      yieldFirst l p = p M.~> M.prepended l

yieldUntilAwait
  :: (Monad m)
  => M.ProcessT m a b
  -> m ([b], a -> M.ProcessT m a b, M.ProcessT m a b)
yieldUntilAwait (M.MachineT ms) = do
  step <- ms
  case step of
    M.Stop      -> return ([], const . M.encased $ M.Stop, M.encased M.Stop)
    M.Yield o r -> do
      (outputs, await, end) <- yieldUntilAwait r
      return (o : outputs, await, end)
    M.Await f M.Refl r -> return ([], f, r)
