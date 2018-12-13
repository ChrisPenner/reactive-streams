{-# LANGUAGE TemplateHaskell #-}
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
