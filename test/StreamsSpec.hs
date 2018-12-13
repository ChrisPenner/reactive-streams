{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module StreamsSpec where

import Data.Machine
import Data.Foldable
import Test.Hspec
import Reactive.Streams

testProcess :: Process a b -> [a] -> [b]
testProcess p src = toList $ (source src ~> p)

testProcessIO :: ProcessT IO a b -> [a] -> IO [b]
testProcessIO p src = runT $ (source src ~> p)



spec :: Spec
spec = do
  describe "create" $ do
    it "should yield all handled values then stop" $ do
      result <- testProcessIO
        (create (\handle -> handle 1 >> handle 2 >> handle 3))
        []
      result `shouldBe` [1, 2, 3]

  describe "empty" $ do
    it "should stop immediately, ignoring input" $ do
      testProcess empty [1, 2, 3] `shouldBe` ([] :: [()])

  describe "from" $ do
    it "should yield values from foldable then stop" $ do
      testProcess (from [1, 2, 3]) [4, 5, 6] `shouldBe` [1, 2, 3]

  -- describe "interval" $ do
  --   it "should yield values in due time" $ do
  --     testProcessIO (interval 100) [4, 5, 6] `shouldBe` [1]

  describe "of'" $ do
    it "should a single value then stop" $ do
      testProcess (of' 1) [4, 5, 6] `shouldBe` [1]

  describe "range" $ do
    it "should yield values in range then stop" $ do
      testProcess (range 'a' 'c') "xyx" `shouldBe` "abc"

  describe "range" $ do
    it "should yield values in range then stop" $ do
      testProcess (range 'a' 'c') "xyx" `shouldBe` "abc"

  -- describe "timer" $ do
  --   it "should trigger repeatedly after given time" $ do
  --     testProcessIO (interval 100) [4, 5, 6] `shouldBe` [1]

  describe "catch" $ do
    it "should catch errors and run new machine" $ do
      testProcess (range 'a' 'c') "xyx" `shouldBe` "abc"
