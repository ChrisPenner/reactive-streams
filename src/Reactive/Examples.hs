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

debouncer :: SourceT IO ()
debouncer = interval 1 ~> debounceTime 1000 ~> autoM print

-- mergeMap :: (a -> IO b) -> ProcessT IO a b
-- mergeMap _ = undefined
