module Cube.Movements (
  frontClockwise, 
  frontCounterClockwise,
  rightClockwise, 
  rightCounterClockwise,
  xClockwise, 
  yClockwise
) where

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
    swapPositions = [6,3,0,7,4,1,8,5,2,42,10,11,43,13,14,44,16,17,18,19,20,21,22,23,24,25,26,27,28,45,30,31,46,33,34,47,36,37,38,39,40,41,35,32,29,15,12,9,48,49,50,51,52,53]

-- F'
frontCounterClockwise :: Cube -> Either String Cube
frontCounterClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [2,5,8,1,4,7,0,3,6,47,10,11,46,13,14,45,16,17,18,19,20,21,22,23,24,25,26,27,28,44,30,31,43,33,34,42,36,37,38,39,40,41,9,12,15,29,32,35,48,49,50,51,52,53]

-- R
rightClockwise :: Cube -> Either String Cube
rightClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [0,1,47,3,4,50,6,7,53,15,12,9,16,13,10,17,14,11,44,19,20,41,22,23,38,25,26,27,28,29,30,31,32,33,34,35,36,37,2,39,40,5,42,43,8,45,46,24,48,49,21,51,52,18]

-- R'
rightCounterClockwise :: Cube -> Either String Cube
rightCounterClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [0,1,38,3,4,41,6,7,44,11,14,17,10,13,16,9,12,15,53,19,20,50,22,23,47,25,26,27,28,29,30,31,32,33,34,35,36,37,24,39,40,21,42,43,18,45,46,2,48,49,5,51,52,8]
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

