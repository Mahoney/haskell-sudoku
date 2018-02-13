module Sudoku.BusinessLogicSpec where

import Test.Hspec

import Sudoku.BusinessLogic

spec :: Spec
spec =
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
