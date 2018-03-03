module Sudoku.BusinessLogicSpec where

import Test.Hspec

import Sudoku.BusinessLogic
import Sudoku.Interfaces

spec :: Spec
spec =
  describe "BusinessLogic" $ do
    describe "inMemoryTransaction" $ do
      it "returns validation failed output if validation fails" $
        let validator unvalidatedInput = Left ("Validation failed for " ++ unvalidatedInput)
            validationFailedFormatter validationFailure = "Errors: " ++ validationFailure
            transaction = inMemoryTransaction validator validationFailedFormatter undefined undefined
        in transaction "some input" `shouldBe` "Errors: Validation failed for some input"

      it "applies the business logic and the formatter to validated input" $
         let validator unvalidatedInput = Right (words unvalidatedInput)
             businessLogic = fmap length
             formatter = unwords . fmap show
             transaction = inMemoryTransaction validator undefined businessLogic formatter
         in transaction "text right here" `shouldBe` "4 5 4"

    describe "solve" $ do
      it "solves a valid known sudoku" $
        let result = solve $ sudoku  [ X,  X,  X,  X,  X,  X, I3,  X,  X,
                                      I6,  X,  X,  X, I2,  X, I1,  X,  X,
                                       X, I3, I2, I8,  X, I7,  X,  X,  X,
                                       X,  X,  X,  X, I4,  X,  X, I6,  X,
                                      I4,  X, I6, I9,  X, I8, I5,  X, I7,
                                       X, I9,  X,  X, I7,  X,  X,  X,  X,
                                       X,  X,  X, I1,  X, I5, I8, I2,  X,
                                       X,  X, I4,  X, I8,  X,  X,  X, I1,
                                       X,  X, I5,  X,  X,  X,  X,  X,  X]
        in result `shouldBe` [sudoku [I1, I4, I8, I6, I5, I9, I3, I7, I2,
                                      I6, I5, I7, I3, I2, I4, I1, I9, I8,
                                      I9, I3, I2, I8, I1, I7, I6, I4, I5,
                                      I7, I8, I3, I5, I4, I1, I2, I6, I9,
                                      I4, I2, I6, I9, I3, I8, I5, I1, I7,
                                      I5, I9, I1, I2, I7, I6, I4, I8, I3,
                                      I3, I7, I9, I1, I6, I5, I8, I2, I4,
                                      I2, I6, I4, I7, I8, I3, I9, I5, I1,
                                      I8, I1, I5, I4, I9, I2, I7, I3, I6]]

      it "solves another valid known sudoku" $
        let result = solve $ sudoku  [ X,  X,  X,  X,  X,  X,  X,  X,  X,
                                      I6,  X,  X, I7, I3,  X,  X,  X,  X,
                                       X, I5, I1,  X,  X, I9,  X,  X,  X,
                                       X,  X, I9, I4,  X, I2, I5,  X,  X,
                                       X,  X,  X, I8,  X,  X,  X, I7,  X,
                                       X, I7,  X,  X, I1, I5,  X, I6,  X,
                                       X, I4,  X,  X,  X, I1, I7,  X,  X,
                                       X, I1, I5, I3,  X,  X, I9,  X,  X,
                                      I7,  X,  X,  X,  X,  X,  X, I1,  X]
        in result `shouldBe` [sudoku [I4, I3, I7, I1, I5, I8, I6, I9, I2,
                                      I6, I9, I2, I7, I3, I4, I8, I5, I1,
                                      I8, I5, I1, I6, I2, I9, I3, I4, I7,
                                      I1, I6, I9, I4, I7, I2, I5, I3, I8,
                                      I5, I2, I4, I8, I6, I3, I1, I7, I9,
                                      I3, I7, I8, I9, I1, I5, I2, I6, I4,
                                      I9, I4, I6, I5, I8, I1, I7, I2, I3,
                                      I2, I1, I5, I3, I4, I7, I9, I8, I6,
                                      I7, I8, I3, I2, I9, I6, I4, I1, I5]]

      it "solves another valid known sudoku with a hint" $
        let result = solve $ sudoku  [ X, I3,  X,  X,  X,  X,  X,  X,  X,
                                      I6,  X,  X, I7, I3,  X,  X,  X,  X,
                                       X, I5, I1,  X,  X, I9,  X,  X,  X,
                                       X,  X, I9, I4,  X, I2, I5,  X,  X,
                                       X,  X,  X, I8,  X,  X,  X, I7,  X,
                                       X, I7,  X,  X, I1, I5,  X, I6,  X,
                                       X, I4,  X,  X,  X, I1, I7,  X,  X,
                                       X, I1, I5, I3,  X,  X, I9,  X,  X,
                                      I7,  X,  X,  X,  X,  X,  X, I1,  X]
        in result `shouldBe` [sudoku [I4, I3, I7, I1, I5, I8, I6, I9, I2,
                                      I6, I9, I2, I7, I3, I4, I8, I5, I1,
                                      I8, I5, I1, I6, I2, I9, I3, I4, I7,
                                      I1, I6, I9, I4, I7, I2, I5, I3, I8,
                                      I5, I2, I4, I8, I6, I3, I1, I7, I9,
                                      I3, I7, I8, I9, I1, I5, I2, I6, I4,
                                      I9, I4, I6, I5, I8, I1, I7, I2, I3,
                                      I2, I1, I5, I3, I4, I7, I9, I8, I6,
                                      I7, I8, I3, I2, I9, I6, I4, I1, I5]]
