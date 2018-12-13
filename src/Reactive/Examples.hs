module Reactive.Examples where

import Data.Machine
import Reactive.Combinators
import Control.Concurrent

printOnDelay :: SourceT IO ()
printOnDelay = create (counter 1) ~> autoM print
 where
  counter i handle = do
    threadDelay 1000000
    handle i
    counter (i + 1) handle

-- mergeMap :: (a -> IO b) -> ProcessT IO a b
-- mergeMap _ = undefined
