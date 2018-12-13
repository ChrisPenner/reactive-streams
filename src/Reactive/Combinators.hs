{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Reactive.Combinators (
  -- * Creation
  create
  ) where

import Prelude hiding (map)
-- import Reactive.Types
import Control.Arrow
import Data.Machine as M
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class

------------------------------------Creation------------------------------------

type Handler m a = a -> m ()
create :: MonadIO m => ((a -> IO ()) -> IO ()) -> SourceT m a
create f = MachineT $ do
  queue <- liftIO . atomically $ newTBQueue (fromIntegral 100)
  let handler = atomically . writeTBQueue queue
  asyncID <- liftIO $ async (f handler)
  continue queue
 where
  continue queue = do
    o <- liftIO . atomically $ readTBQueue queue
    return $ Yield o (MachineT $ continue queue)

------------------------------------Other---------------------------------------
-- continually :: (Monad m) => m a -> SourceT m a
-- continually ma = StreamT . M.MachineT $ (yield <$> ma)
--   where yield a = M.Yield a (toProcess $ continually ma)

-- map :: (Monad m) => (a -> m b) -> StreamT m a b
-- map f = StreamT $ M.autoT (Kleisli f)

-- runStream :: Monad m => StreamT m a b -> m [b]
-- runStream (StreamT m) = M.runT m

-- sampleBehaviour :: (Monad m) => m b -> StreamT m a b
-- sampleBehaviour m = map (const m)

-- alongside :: Monad m => (a -> m b) -> StreamT m a (a, b)
-- alongside f = map (\a -> (a, ) <$> f a)

-- excite :: (Monad m) => StreamT m String String
-- excite = map (pure . (++ "!!"))

-- source :: (Monad m, Foldable f) => f b -> StreamT m a b
-- source = StreamT . M.source

-- -- concat :: (Monad m) => StreamT m a b -> StreamT m a b -> StreamT m a b
-- -- concat m n = 
