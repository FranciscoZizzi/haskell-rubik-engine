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

-- U = 3X F X
generateUpClockwiseSwap :: [Int]
generateUpClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- xClockwise testCube
      result <- xClockwise result
      result <- xClockwise result
      result <- frontClockwise result
      xClockwise result

-- U' = 3U
generateUpCounterClockwiseSwap :: [Int]
generateUpCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- upClockwise testCube
      result <- upClockwise result
      upClockwise result

-- B = 2Y F 2Y
generateBackClockwiseSwap :: [Int]
generateBackClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- yClockwise testCube
      result <- yClockwise result
      result <- frontClockwise result
      result <- yClockwise result
      yClockwise result

-- B' = 3B
generateBackCounterClockwiseSwap :: [Int]
generateBackCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- backClockwise testCube
      result <- backClockwise result
      backClockwise result

-- L = 3Y F Y
generateLeftClockwiseSwap :: [Int]
generateLeftClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- yClockwise testCube
      result <- yClockwise result
      result <- yClockwise result
      result <- frontClockwise result
      yClockwise result

-- L' = 3L
generateLeftCounterClockwiseSwap :: [Int]
generateLeftCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- leftClockwise testCube
      result <- leftClockwise result
      leftClockwise result

-- M' = R' L X
generateMCounterClockwiseSwap :: [Int]
generateMCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- rightCounterClockwise testCube
      result <- leftClockwise result
      xClockwise result

-- M = 3M'
generateMClockwiseSwap :: [Int]
generateMClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- middleCounterclockwise testCube
      result <- middleCounterclockwise result
      middleCounterclockwise result

-- X' = 3X
generateXCounterClockwiseSwap :: [Int]
generateXCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- xClockwise testCube
      result <- xClockwise result
      xClockwise result

-- Y' = 3Y
generateYCounterClockwiseSwap :: [Int]
generateYCounterClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- yClockwise testCube
      result <- yClockwise result
      yClockwise result

-- Z = Y X' Y'
generateZClockwiseSwap :: [Int]
generateZClockwiseSwap = toList (faces resultCube)
  where
    (Right resultCube) = do
      result <- yClockwise testCube
      result <- xCounterclockwise result
      yCounterclockwise result

-- Z' = 3Z
generateZCounterClockwiseSwap :: [Int]
generateZCounterClockwiseSwap = toList (faces resultCube)
 where
   (Right resultCube) = do
     result <- zClockwise testCube
     result <- zClockwise result
     zClockwise result
