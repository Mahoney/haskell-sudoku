module Sudoku.SimpleCli.ValidationSpec where

import Test.Hspec
import Sudoku.Interfaces
import Sudoku.SimpleCli.Validation

spec :: Spec
spec = describe "validate" $ do
         it "returns a validation error if no input" $
           validate [] `shouldBe` Left "Takes one argument of type sudoku"

         it "returns a validation error if more than one input" $
           validate [
             ".................................................................................",
             "................................................................................."
             ] `shouldBe` Left "Takes one argument of type sudoku"

         it "returns a validation error if input less than 81 chars long" $
           validate ["................................................................................"]
             `shouldBe` Left "A valid sudoku is composed of 81 characters, each either a period (.) or 1-9; yours had 80"

         it "returns a validation error if input more than 81 chars long" $
           validate [".................................................................................."]
             `shouldBe` Left "A valid sudoku is composed of 81 characters, each either a period (.) or 1-9; yours had 82"

         it "returns a valid empty sudoku" $
           validate ["................................................................................."]
             `shouldBe` Right emptyGrid

         it "returns a validation error if input contains invalid characters" $
           validate ["1.a.Z.2.........................................................................."]
             `shouldBe` Left "a is not . or 1-9; Z is not . or 1-9"

         it "returns a valid grid with some cells filled in" $
           validate ["......3.."++
                     "6...2.1.."++
                     ".328.7..."++
                     "....4..6."++
                     "4.69.85.7"++
                     ".9..7...."++
                     "...1.582."++
                     "..4.8...1"++
                     "..5......"]
             `shouldBe` Right (Sudoku [
               empty A R1  , empty B R1  , empty C R1  , empty D R1  , empty E R1  , empty F R1  , inked G R1 3, empty H R1  , empty I R1  ,
               inked A R2 6, empty B R2  , empty C R2  , empty D R2  , inked E R2 2, empty F R2  , inked G R2 1, empty H R2  , empty I R2  ,
               empty A R3  , inked B R3 3, inked C R3 2, inked D R3 8, empty E R3  , inked F R3 7, empty G R3  , empty H R3  , empty I R3  ,
               empty A R4  , empty B R4  , empty C R4  , empty D R4  , inked E R4 4, empty F R4  , empty G R4  , inked H R4 6, empty I R4  ,
               inked A R5 4, empty B R5  , inked C R5 6, inked D R5 9, empty E R5  , inked F R5 8, inked G R5 5, empty H R5  , inked I R5 7,
               empty A R6  , inked B R6 9, empty C R6  , empty D R6  , inked E R6 7, empty F R6  , empty G R6  , empty H R6  , empty I R6  ,
               empty A R7  , empty B R7  , empty C R7  , inked D R7 1, empty E R7  , inked F R7 5, inked G R7 8, inked H R7 2, empty I R7  ,
               empty A R8  , empty B R8  , inked C R8 4, empty D R8  , inked E R8 8, empty F R8  , empty G R8  , empty H R8  , inked I R8 1,
               empty A R9  , empty B R9  , inked C R9 5, empty D R9  , empty E R9  , empty F R9  , empty G R9  , empty H R9  , empty I R9
             ])
