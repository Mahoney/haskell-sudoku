import Test.Hspec

import qualified Sudoku.CliSpec
import qualified Sudoku.BusinessLogicSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cli"           Sudoku.CliSpec.spec
  describe "BusinessLogic" Sudoku.BusinessLogicSpec.spec
