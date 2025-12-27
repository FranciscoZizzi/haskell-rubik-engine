import Test.Hspec
import Control.Monad
import Cube.Data
import Cube.DefaultCubes

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Movements" $ do
  it "should result in solved cube if same face rotated 4 times" $ do
    fmap isSolved (executeMovements ["F","F","F","F"] defaultCube) `shouldBe` Right True
  it "should result in equal cube if same face rotated 4 times" $ do
    executeMovements ["F","F","F","F"] defaultCube `shouldBe` Right defaultCube

executeMovements :: [String] -> Cube -> Either String Cube
executeMovements movements cube = foldM executeMovement cube movements
