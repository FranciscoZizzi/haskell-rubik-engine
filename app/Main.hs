module Main (main) where

import Cube.Data

main :: IO ()
main = print (getLeftFace defaultCube)
