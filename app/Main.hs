module Main (main) where

import Cube.Data
import UI.CMD
import Cube.Movements

main :: IO ()
main = printCube cube
  where
    cube = case zClockwise $ defaultCube of
      Right result -> result
      Left _ -> defaultCube
