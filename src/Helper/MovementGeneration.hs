module Helper.MovementGeneration where

import Cube.Data
import Cube.Movements
import Data.Vector

testCube :: Cube
testCube = Cube (fromList [0..53]) 3

-- R = Y F 3Y
generateRightClockwiseSwap :: [Int] 
generateRightClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- yClockwise testCube
      result <- frontClockwise result
      result <- yClockwise result
      result <- yClockwise result
      yClockwise result

-- R' = 3R
generateRightCounterClockwiseSwap :: [Int]
generateRightCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- rightClockwise testCube
      result <- rightClockwise result
      rightClockwise result

-- F' = 3F
generateFrontCounterClockwiseSwap :: [Int]
generateFrontCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- frontClockwise testCube
      result <- frontClockwise result
      frontClockwise result
