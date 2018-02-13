import Test.Hspec

import qualified Sudoku.SimpleCli.CliSpec
import qualified Sudoku.SimpleCli.ValidationSpec
import qualified Sudoku.BusinessLogicSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cli"           Sudoku.SimpleCli.CliSpec.spec
  describe "BusinessLogic" Sudoku.BusinessLogicSpec.spec
  describe "Validation"    Sudoku.SimpleCli.ValidationSpec.spec
