import Test.Hspec
import Sudoku.Cli (app)

main :: IO ()
main = hspec $
  describe "Main app" $
    it "solves a hard sudoku" $
      app ["................................................................................."] `shouldBe` "[Sudoku \".................................................................................\"]"
