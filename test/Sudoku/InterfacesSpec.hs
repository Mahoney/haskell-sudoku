module Sudoku.InterfacesSpec where

import Test.Hspec

import Sudoku.Interfaces

spec :: Spec
spec =
  describe "Sudoku" $ do
    describe "show" $ do
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

    describe "columns" $
      it "groups cells by column" $
        columns (Sudoku
                  [Cell (A, R1) [1], Cell (B, R1) [2],
                   Cell (A, R2) [3], Cell (B, R2) [4]])
        `shouldBe` [[Cell (A, R1) [1], Cell (A, R2) [3]],
                    [Cell (B, R1) [2], Cell (B, R2) [4]]]

    describe "squares" $ do
      it "groups cells by square and makes 9" $
        length (squares emptyGrid) `shouldBe` 9

      it "groups cells by square and makes 9" $
        all (\square -> length square == 9) (squares emptyGrid)

      it "groups cells by square checking first" $
        head (squares emptyGrid)
        `shouldBe`
        [empty A R1, empty B R1, empty C R1,
         empty A R2, empty B R2, empty C R2,
         empty A R3, empty B R3, empty C R3]

      it "groups cells by square checking last" $
        last (squares emptyGrid)
        `shouldBe`
        [empty G R7, empty H R7, empty I R7,
         empty G R8, empty H R8, empty I R8,
         empty G R9, empty H R9, empty I R9]
