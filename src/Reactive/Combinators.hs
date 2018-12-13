{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Reactive.Combinators where

import Reactive.Types
import Control.Arrow
import qualified Data.Machine as M

continually :: (Monad m) => m a -> SourceT m a
continually ma = StreamT . M.MachineT $ (yield <$> ma)
  where
    yield a = M.Yield a (toProcess $ continually ma)

mapME :: (Monad m) => (a -> m b) -> StreamT m a b
mapME f = StreamT $ M.autoT (Kleisli f)

runStream :: Monad m => StreamT m a b -> m [b]
runStream (StreamT m) = M.runT m

readLineE :: SourceT IO String
readLineE = continually getLine

sampleBehaviour :: (Monad m) => m b -> StreamT m a b
sampleBehaviour m = mapME (const m)

alongside :: Monad m => (a -> m b) -> StreamT m a (a, b)
alongside f = mapME (\a -> (a, ) <$> f a)

excite :: (Monad m) => StreamT m String String
excite = mapME (pure . (++ "!!"))

printE :: StreamT IO String ()
printE = mapME print

example :: StreamT IO String ()
example = proc x -> do
  out <- excite -< x
  printE -< out

source :: (Monad m, Foldable f) => f b -> StreamT m a b
source = StreamT . M.source
