module Cube.Data (
  Cube(..),
  defaultCube,
  createCube,
  getStickerColor,
  getFrontFace,
  getLeftFace,
  getRightFace,
  getDownFace,
  getUpFace,
  getBackFace
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

getStickerColor :: Cube -> Int -> Int
getStickerColor cube stickerPosition = faces cube ! stickerPosition

getNthFace :: Cube -> Int -> Vector Int
getNthFace (Cube f fs) n = slice (n * fs * fs) (fs * fs) f

getFrontFace :: Cube -> Vector Int
getFrontFace cube = getNthFace cube 0

getRightFace :: Cube -> Vector Int
getRightFace cube = getNthFace cube 1

getBackFace :: Cube -> Vector Int
getBackFace cube = getNthFace cube 2

getLeftFace :: Cube -> Vector Int
getLeftFace cube = getNthFace cube 3

getUpFace :: Cube -> Vector Int
getUpFace cube = getNthFace cube 4

getDownFace :: Cube -> Vector Int
getDownFace cube = getNthFace cube 5
