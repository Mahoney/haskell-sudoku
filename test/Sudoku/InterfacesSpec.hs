module Sudoku.InterfacesSpec where

import Test.Hspec

import Sudoku.Interfaces

spec :: Spec
spec =
  describe "show Sudoku" $ do
    it "prints an empty sudoku" $
      show emptyGrid `shouldBe` "\n+-------+-------+-------+"++
                                "\n| . . . | . . . | . . . |"++
                                "\n| . . . | . . . | . . . |"++
                                "\n| . . . | . . . | . . . |"++
                                "\n+-------+-------+-------+"++
                                "\n| . . . | . . . | . . . |"++
                                "\n| . . . | . . . | . . . |"++
                                "\n| . . . | . . . | . . . |"++
                                "\n+-------+-------+-------+"++
                                "\n| . . . | . . . | . . . |"++
                                "\n| . . . | . . . | . . . |"++
                                "\n| . . . | . . . | . . . |"++
                                "\n+-------+-------+-------+\n"

    it "prints a sudoku with values" $
      let grid = sudoku [ X,  X,  X,  X,  X,  X, I3,  X,  X,
                         I6,  X,  X,  X, I2,  X, I1,  X,  X,
                          X, I3, I2, I8,  X, I7,  X,  X,  X,
                          X,  X,  X,  X, I4,  X,  X, I6,  X,
                         I4,  X, I6, I9,  X, I8, I5,  X, I7,
                          X, I9,  X,  X, I7,  X,  X,  X,  X,
                          X,  X,  X, I1,  X, I5, I8, I2,  X,
                          X,  X, I4,  X, I8,  X,  X,  X, I1,
                          X,  X, I5,  X,  X,  X,  X,  X,  X]
      in show grid `shouldBe` "\n+-------+-------+-------+"++
                              "\n| . . . | . . . | 3 . . |"++
                              "\n| 6 . . | . 2 . | 1 . . |"++
                              "\n| . 3 2 | 8 . 7 | . . . |"++
                              "\n+-------+-------+-------+"++
                              "\n| . . . | . 4 . | . 6 . |"++
                              "\n| 4 . 6 | 9 . 8 | 5 . 7 |"++
                              "\n| . 9 . | . 7 . | . . . |"++
                              "\n+-------+-------+-------+"++
                              "\n| . . . | 1 . 5 | 8 2 . |"++
                              "\n| . . 4 | . 8 . | . . 1 |"++
                              "\n| . . 5 | . . . | . . . |"++
                              "\n+-------+-------+-------+\n"
