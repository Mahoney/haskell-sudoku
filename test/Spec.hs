import Test.Hspec

import qualified Sudoku.SimpleCli.CliSpec
import qualified Sudoku.SimpleCli.ValidationSpec
import qualified Sudoku.BusinessLogicSpec
import qualified Sudoku.InterfacesSpec
import qualified Ext.Data.EitherSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cli"             Sudoku.SimpleCli.CliSpec.spec
  describe "BusinessLogic"   Sudoku.BusinessLogicSpec.spec
  describe "Interfaces"      Sudoku.InterfacesSpec.spec
  describe "Validation"      Sudoku.SimpleCli.ValidationSpec.spec
  describe "Ext.Data.Either" Ext.Data.EitherSpec.spec
