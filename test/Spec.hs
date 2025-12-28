import Test.Hspec
import Control.Monad
import Cube.Data
import Cube.DefaultCubes

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Movements" $ do
    it "should result in solved cube if same face rotated 4 times" $ do
      fmap isSolved (executeMovements ["F","F","F","F"] defaultCube) `shouldBe` Right True
    it "should result in equal cube if F face rotated 4 times" $ do
      executeMovements ["F","F","F","F"] defaultCube `shouldBe` Right defaultCube
    it "should result in equal cube if U face rotated 4 times" $ do
      executeMovements ["U","U","U","U"] defaultCube `shouldBe` Right defaultCube
    it "should result in solved cube if sequence of order 6 is repeated 6 times" $ do
      executeMovements cyclicMovement defaultCube `shouldBe` Right defaultCube
  describe "isSolved" $ do
    it "should return true for a new cube" $ do
      isSolved defaultCube `shouldBe` True
    it "should return false for cube with not in initial state" $ do
      fmap isSolved (executeMovements ["F", "U", "U", "F"] defaultCube) `shouldBe` Right False
    it "should return true for cube with movements applied but in solved state" $ do
      fmap isSolved (executeMovements cyclicMovement defaultCube) `shouldBe` Right True

executeMovements :: [String] -> Cube -> Either String Cube
executeMovements movements cube = foldM executeMovement cube movements

cyclicMovement :: [String]
cyclicMovement = [
  "F","U","F","F","F","U","U","U",
  "F","U","F","F","F","U","U","U",
  "F","U","F","F","F","U","U","U",
  "F","U","F","F","F","U","U","U",
  "F","U","F","F","F","U","U","U",
  "F","U","F","F","F","U","U","U"]

