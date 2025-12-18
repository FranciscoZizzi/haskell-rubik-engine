module Cube.Data (
  Cube(..),
  defaultCube,
  createCube,
  getFrontSide,
  getLeftSide,
  getRightSide,
  getDownSide,
  getUpSide,
  getBackSide
) where

import Data.Vector

data Cube = Cube {
  faces :: Vector Int,
  faceSize :: Int
}

defaultCube :: Cube
defaultCube = createCube 3

createCube :: Int -> Cube
createCube size = Cube (fromList facesList) size
  where
    facesList = [y | x <- [0..5], y <- Prelude.replicate (size * size) x]

getNthSide :: Cube -> Int -> Vector Int
getNthSide (Cube f fs) n = slice (n * fs * fs) (fs * fs) f

getFrontSide :: Cube -> Vector Int
getFrontSide cube = getNthSide cube 0

getRightSide :: Cube -> Vector Int
getRightSide cube = getNthSide cube 1

getBackSide :: Cube -> Vector Int
getBackSide cube = getNthSide cube 2

getLeftSide :: Cube -> Vector Int
getLeftSide cube = getNthSide cube 3

getUpSide :: Cube -> Vector Int
getUpSide cube = getNthSide cube 4

getDownSide :: Cube -> Vector Int
getDownSide cube = getNthSide cube 5
