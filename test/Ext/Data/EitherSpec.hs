module Ext.Data.EitherSpec where

import Test.Hspec
import Ext.Data.Either

spec :: Spec
spec = describe "split" $ do

         it "returns an empty right for empty input" $
           split ([] :: [Either Int Int]) `shouldBe` Right []

         it "gathers all rights together" $
           split ([Right 1, Right 2] :: [Either Int Int]) `shouldBe` Right [1, 2]

         it "gathers all lefts together" $
           split ([Left 1, Left 2] :: [Either Int Int]) `shouldBe` Left [1, 2]

         it "gathers some lefts together" $
           split [Left '1', Right '2'] `shouldBe` Left ['1']

         it "gathers some lefts together" $
           split [Right '1', Left '2'] `shouldBe` Left ['2']
