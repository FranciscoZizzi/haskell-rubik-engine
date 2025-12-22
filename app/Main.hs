module Main (main) where

import Cube.Data
import UI.CMD
import Cube.Movements

main :: IO ()
main = mainLoop defaultCube


thing :: IO ()
thing = printCube cube
  where
    cube = case applyMovements crossPattern defaultCube of
      Right result -> result
      Left _ -> defaultCube

crossPattern :: [Movement]
crossPattern = [middleClockwise, middleClockwise, yClockwise, middleClockwise, middleClockwise, zClockwise, middleClockwise, middleClockwise]
