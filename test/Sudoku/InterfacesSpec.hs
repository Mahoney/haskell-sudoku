module Sudoku.InterfacesSpec where

import Test.Hspec

import Sudoku.Interfaces
import Data.Set (elemAt, fromList)

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
        columns (makeSudoku
                  [cell A C1 [1], cell A C2 [2],
                   cell B C1 [3], cell B C2 [4]])
        `shouldBe` fromList [ fromList [cell A C1 [1], cell B C1 [3]],
                              fromList [cell A C2 [2], cell B C2 [4]]]

    describe "squares" $ do
      it "groups cells by square and makes 9" $
        length (squares emptyGrid) `shouldBe` 9

      it "groups cells by square and makes 9" $
        all (\square -> length square == 9) (squares emptyGrid)

      it "groups cells by square checking first" $
        elemAt 0 (squares emptyGrid)
        `shouldBe`
        fromList [empty A C1, empty A C2, empty A C3,
         empty B C1, empty B C2, empty B C3,
         empty C C1, empty C C2, empty C C3]

      it "groups cells by square checking last" $
        elemAt 8 (squares emptyGrid)
        `shouldBe`
        fromList [empty G C7, empty G C8, empty G C9,
                  empty H C7, empty H C8, empty H C9,
                  empty I C7, empty I C8, empty I C9]
