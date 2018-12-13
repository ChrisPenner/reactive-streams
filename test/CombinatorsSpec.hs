{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
module CombinatorsSpec where

import Data.Functor.Identity
import Test.Hspec
import Data.Char
import Reactive.Types
import Reactive.Combinators
import Control.Category
import Control.Arrow
import Control.Monad.State

testProcess :: StreamT Identity a b -> [a] -> [b]
testProcess p src = runIdentity $ runStream (source src >>> p)

testProcessM :: (Monad m) => StreamT m a b -> [a] -> m [b]
testProcessM p src = runStream (source src >>> p)


spec :: Spec
spec = do
  describe "Behaviours" $ do
    it "sampleBehaviour should sample once per event" $ do
      flip
          evalState
          "abcdef"
          (testProcessM (sampleBehaviour (gets head <* modify tail)) [1, 2, 3])
        `shouldBe` "abc"
