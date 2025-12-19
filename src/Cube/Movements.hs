module Cube.Movements (frontClockwise) where

import Cube.Data (Cube(..))
import Data.Vector

executeMove :: Cube -> [Int] -> Either String Cube
executeMove (Cube f 3) swapPositions = Right (Cube result 3)
  where
    result = fromList [f ! x | x <- swapPositions]
executeMove (Cube _ _) _ = Left "Movements for cubes of size != 3 not supported"

-- F
frontClockwise :: Cube -> Either String Cube
frontClockwise cube = executeMove cube swapPositions
  where 
    swapPositions = [6,3,0,7,4,1,8,5,2,42,10,11,43,13,14,44,16,17,18,19,20,21,22,23,24,25,26,27,28,45,30,31,46,33,34,47,36,37,38,39,40,41,35,32,29,15,12,9,48,49,50,51,52,53] :: [Int]

-- X
xClockwise :: Cube -> Either String Cube
xClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [45,46,47,48,49,50,51,52,53,15,12,9,16,13,10,17,14,11,36,37,38,39,40,41,42,43,44,29,32,35,28,31,34,27,30,33,0,1,2,3,4,5,6,7,8,18,19,20,21,22,23,24,25,26]

-- Y
yClockwise :: Cube -> Either String Cube
yClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,00,01,02,03,04,05,06,07,08,42,39,36,43,40,37,44,41,38,47,50,53,46,49,52,45,48,51]

