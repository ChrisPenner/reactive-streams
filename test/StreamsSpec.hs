{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
module TypesSpec where

import Data.Functor.Identity
import Test.Hspec
import Data.Char
import Reactive.Types
import Reactive.Combinators
import Control.Category
import Control.Arrow

testProcess :: StreamT Identity a b -> [a] -> [b]
testProcess p src = runIdentity $ runStream (source src >>> p)

spec :: Spec
spec = do
  describe "Arrow" $ do
    it "should lift functions with arr" $ do
      testProcess (arr toUpper) "ab" `shouldBe` "AB"
    it "should map first arg using 'first'" $ do
      testProcess (first $ arr toUpper) [('a', 1), ('b', 2)]
        `shouldBe` [('A', 1), ('B', 2)]
    it "should pair second of tuple with original input until an await" $ do
      testProcess (first $ source [1, 2, 3]) [(99, 'a')]
        `shouldBe` [(1, 'a'), (2, 'a'), (3, 'a')]
    it "works with arrow notation" $ do
      let stream = proc x -> do
                    succX <- (arr (+1)) -< x
                    multX <- (arr (*100)) -< x
                    returnA -< (succX, multX)
      testProcess stream [1, 2, 3]
        `shouldBe` [(2, 100), (3, 200), (4, 300)]
      -- let stream2 = proc x -> do
      --               succX <- (arr (+1)) -< x
      --               multX <- (arr (*100)) -< x
      --               letter <- (source @Identity "abcd" ) -< x
      --               returnA -< (succX, multX, letter)
      -- testProcess stream2 [1, 2, 3]
      --   `shouldBe` [(2, 100, 'a'), (3, 200, 'b'), (4, 300, 'c')]
