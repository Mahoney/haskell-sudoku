module Ext.Data.EitherSpec where

import Test.Hspec
import Ext.Data.Either

spec :: Spec
spec = describe "leftPartition" $ do

         it "returns an empty right for empty input" $
           leftPartition ([] :: [Either Int Int]) `shouldBe` Right []

         it "gathers all rights together" $
           leftPartition ([Right 1, Right 2] :: [Either Int Int]) `shouldBe` Right [1, 2]

         it "gathers all lefts together" $
           leftPartition ([Left 1, Left 2] :: [Either Int Int]) `shouldBe` Left [1, 2]

         it "gathers some lefts together" $
           leftPartition [Left '1', Right '2'] `shouldBe` Left ['1']

         it "gathers some lefts together" $
           leftPartition [Right '1', Left '2'] `shouldBe` Left ['2']
