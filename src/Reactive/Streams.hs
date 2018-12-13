{-# LANGUAGE RankNTypes #-}
module Reactive.Streams (
  -- * Creation
  create
                            , empty
                            , from
                            , of'
                            , interval
                            , range
                            , wait
                            , timer
  -- * Error Handling
                            , catch
                            , retry
  -- * Filtering
                            , debounceTime
                            , distinctUntilChanged
                            , Reactive.Streams.filter
                            , Reactive.Streams.first
                            , firstOf
                            , ignoreElements
                            , Reactive.Streams.last
                            , sampleM
                            , skip
                            , skipWhile
                            , skipUntil
                            , Reactive.Streams.take
                            , Reactive.Streams.takeWhile
                            , takeUntil

  -- * Transforming
                            , bufferCount
                            , Reactive.Streams.concat
                            , expand
                            , expandM
                            , groupBy
                            , map
                            , map'
                            , mapTo
                            , partition
                            , reduce
                            , reduce1
                            , scan
                            , scan1
  -- * Utility
                            , tap
                            , Reactive.Streams.repeat
  ) where

import Prelude hiding (map)
import Data.Machine as M
import Data.Machine.Lift as M
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async hiding (wait)
import Control.Concurrent.STM hiding (retry)
import Control.Monad.IO.Class
import Control.Monad.Except hiding (mapM)
import Control.Monad.State hiding (mapM)
import Data.Functor
import qualified Data.Map as M

------------------------------------Creation------------------------------------

type Handler m a = a -> m ()
create :: MonadIO m => ((a -> IO ()) -> IO ()) -> SourceT m a
create f = MachineT $ do
  queue <- liftIO . atomically $ newTBQueue 100
  let handler = atomically . writeTBQueue queue
  asyncID <- liftIO $ async (f handler)
  continue queue
 where
  continue queue = do
    o <- liftIO . atomically $ readTBQueue queue
    return $ Yield o (MachineT $ continue queue)

empty :: Machine k b
empty = stopped

from :: Foldable f => f a -> Source a
from = source

interval :: MonadIO m => Int -> SourceT m Int
interval gapMillis = create (counter 0)
 where
  counter i handle = do
    threadDelay (1000 * gapMillis)
    handle i
    counter (i + 1) handle

of' :: a -> Source a
of' a = source [a]

range :: Enum a => a -> a -> Source a
range = enumerateFromTo

wait :: MonadIO m => Int -> SourceT m ()
wait waitMillis = create waiter
  where waiter handle = threadDelay (1000 * waitMillis) >> handle ()

timer :: MonadIO m => Int -> SourceT m ()
timer waitMillis = interval waitMillis $> ()

------------------------------------Error Handling------------------------------

catch
  :: MonadError e m => (e -> MachineT m k b) -> MachineT m k b -> MachineT m k b
catch handler (MachineT m) = MachineT (m `catchError` fmap runMachineT handler)

retry :: (MonadError e m) => Int -> MachineT m k b -> MachineT m k b
retry times m | times <= 0 = m
              | otherwise  = retry (times - 1) $ catch (const m) m

------------------------------------Filtering-----------------------------------

debounceTime :: MonadIO m => Int -> ProcessT m a a
debounceTime time = construct $ do
  lock <- liftIO $ newTVarIO True
  loop lock
 where
  loop lock = do
    v     <- await
    open  <- liftIO $ readTVarIO lock
    lock' <- if open
      then do
-- start debounce timer before yielding in case consumer is slow
        liftIO $ atomically (writeTVar lock False)
        lock' <- liftIO $ registerDelay (time * 1000)
        yield v
        return lock'
      else return lock
    loop lock'

distinctUntilChanged :: (Eq a) => Process a a
distinctUntilChanged = construct $ helper Nothing
 where
  helper last = do
    next <- await
    when (Just next /= last) (yield next)
    helper (Just next)

filter :: (a -> Bool) -> Process a a
filter = filtered

first :: Process a a
first = taking 1

firstOf :: (a -> Bool) -> Process a a
firstOf f = filtered f ~> first

ignoreElements :: Process a a
ignoreElements = filtered (const False)

last :: Process a a
last = final

-- sample :: Source b -> Process a b
-- sample 

sampleM :: (Monad m) => m b -> ProcessT m a b
sampleM = autoM . const

skip :: Int -> Process a a
skip = dropping

skipWhile :: (a -> Bool) -> Process a a
skipWhile = droppingWhile

skipUntil :: (a -> Bool) -> Process a a
skipUntil f = droppingWhile (not . f)

take :: Int -> Process a a
take = taking

takeWhile :: (a -> Bool) -> Process a a
takeWhile = takingWhile

takeUntil :: (a -> Bool) -> Process a a
takeUntil f = takingWhile (not . f)

-- throttleTime :: MonadIO m => Int -> ProcessT m a a
-- throttleTime time = construct $ do

------------------------------------Transforming--------------------------------

bufferCount :: Int -> Process a [a]
bufferCount = buffered

-- buffer :: Source x -> Process a [a]
-- bufferTime :: Int -> Process a [a]
-- bufferToggle :: Source x -> Source y -> Process a [a]
-- bufferWhen :: (x -> Bool) -> Source x -> Process a [a]


concat :: Process a b -> Process a b -> Process a b
concat = (<>)

expand :: (a -> a) -> a -> Source a
expand = iterated

expandM :: Monad m => (a -> m a) -> a -> SourceT m a
expandM f = construct . iter
 where
  iter a = do
    yield a
    a' <- lift $ f a
    iter a'


-- | Collect values into a map keyed by the given selector until the source
-- stops, then emit the map of values
groupBy :: (Ord b) => (a -> b) -> Process a (M.Map b [a])
groupBy selector = fold collect mempty
  where collect m next = M.insertWith (<>) (selector next) [next] m

map :: (a -> b) -> Process a b
map = auto

map' :: Monad m => (a -> m b) -> ProcessT m a b
map' = autoM

mapTo :: b -> Process a b
mapTo = map . const

-- mergeMap :: MonadIO m => (a -> m b) -> Process a b

partition
  :: (Monad m)
  => (b -> Bool)
  -> MachineT m k b
  -> (MachineT m k b, MachineT m k b)
partition pred m = (m ~> filtered pred, m ~> filtered (not . pred))

reduce :: (b -> a -> b) -> b -> Process a b
reduce = fold

reduce1 :: (a -> a -> a) -> Process a a
reduce1 = fold1

-- switchMap :: MonadIO m => (a -> m b) -> Process a b

------------------------------------Utility------------------------------------

tap :: Monad m => (a -> m ()) -> ProcessT m a a
tap f = map' (\a -> f a $> a)

repeat :: Int -> Process a b -> Process a b
repeat times m = mconcat (replicate times m)

-- delay :: MonadIO m => Int -> ProcessT m a a
