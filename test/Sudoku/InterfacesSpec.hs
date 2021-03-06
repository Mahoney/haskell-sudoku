module Sudoku.InterfacesSpec where

import Test.Hspec

import Sudoku.Interfaces
import Data.Set (elemAt, fromList)

spec :: Spec
spec =
  describe "Sudoku" $ do
    describe "show" $ do
      it "prints an solved sudoku" $
        let fullGrid = sudoku [I1, I4, I8, I6, I5, I9, I3, I7, I2,
                               I6, I5, I7, I3, I2, I4, I1, I9, I8,
                               I9, I3, I2, I8, I1, I7, I6, I4, I5,
                               I7, I8, I3, I5, I4, I1, I2, I6, I9,
                               I4, I2, I6, I9, I3, I8, I5, I1, I7,
                               I5, I9, I1, I2, I7, I6, I4, I8, I3,
                               I3, I7, I9, I1, I6, I5, I8, I2, I4,
                               I2, I6, I4, I7, I8, I3, I9, I5, I1,
                               I8, I1, I5, I4, I9, I2, I7, I3, I6]
        in show fullGrid `shouldBe` "\n+-------+-------+-------+"++
                                    "\n| 1 4 8 | 6 5 9 | 3 7 2 |"++
                                    "\n| 6 5 7 | 3 2 4 | 1 9 8 |"++
                                    "\n| 9 3 2 | 8 1 7 | 6 4 5 |"++
                                    "\n+-------+-------+-------+"++
                                    "\n| 7 8 3 | 5 4 1 | 2 6 9 |"++
                                    "\n| 4 2 6 | 9 3 8 | 5 1 7 |"++
                                    "\n| 5 9 1 | 2 7 6 | 4 8 3 |"++
                                    "\n+-------+-------+-------+"++
                                    "\n| 3 7 9 | 1 6 5 | 8 2 4 |"++
                                    "\n| 2 6 4 | 7 8 3 | 9 5 1 |"++
                                    "\n| 8 1 5 | 4 9 2 | 7 3 6 |"++
                                    "\n+-------+-------+-------+\n"

      it "prints an empty sudoku" $
        show emptyGrid `shouldBe` "\n+-------------------------------------------------------+-------------------------------------------------------+-------------------------------------------------------+"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n+-------------------------------------------------------+-------------------------------------------------------+-------------------------------------------------------+"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n+-------------------------------------------------------+-------------------------------------------------------+-------------------------------------------------------+"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n| 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 | 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 1,2,3,4,5,6,7,8,9 |"++
                                  "\n+-------------------------------------------------------+-------------------------------------------------------+-------------------------------------------------------+\n"

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
