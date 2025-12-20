module Cube.Movements (
  frontClockwise, 
  frontCounterClockwise,
  rightClockwise, 
  rightCounterClockwise,
  upClockwise,
  upCounterclockwise,
  backClockwise,
  backCounterclockwise,
  leftClockwise,
  leftCounterclockwise,
  middleClockwise,
  middleCounterclockwise,
  xClockwise,
  xCounterclockwise,
  yClockwise,
  yCounterclockwise,
  zClockwise,
  zCounterclockwise
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

-- U
upClockwise :: Cube -> Either String Cube
upClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [9,10,11,3,4,5,6,7,8,26,25,24,12,13,14,15,16,17,18,19,20,21,22,23,29,28,27,0,1,2,30,31,32,33,34,35,42,39,36,43,40,37,44,41,38,45,46,47,48,49,50,51,52,53]

-- U'
upCounterclockwise :: Cube -> Either String Cube
upCounterclockwise cube = executeMove cube swapPositions
  where
    swapPositions = [27,28,29,3,4,5,6,7,8,0,1,2,12,13,14,15,16,17,18,19,20,21,22,23,11,10,9,26,25,24,30,31,32,33,34,35,38,41,44,37,40,43,36,39,42,45,46,47,48,49,50,51,52,53]

-- B
backClockwise :: Cube -> Either String Cube
backClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [0,1,2,3,4,5,6,7,8,9,10,53,12,13,52,15,16,51,24,21,18,25,22,19,26,23,20,38,28,29,37,31,32,36,34,35,11,14,17,39,40,41,42,43,44,45,46,47,48,49,50,27,30,33]

-- B'
backCounterclockwise :: Cube -> Either String Cube
backCounterclockwise cube = executeMove cube swapPositions
  where
    swapPositions = [0,1,2,3,4,5,6,7,8,9,10,36,12,13,37,15,16,38,20,23,26,19,22,25,18,21,24,51,28,29,52,31,32,53,34,35,33,30,27,39,40,41,42,43,44,45,46,47,48,49,50,17,14,11]

-- L
leftClockwise :: Cube -> Either String Cube
leftClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [36,1,2,39,4,5,42,7,8,9,10,11,12,13,14,15,16,17,18,19,51,21,22,48,24,25,45,33,30,27,34,31,28,35,32,29,26,37,38,23,40,41,20,43,44,0,46,47,3,49,50,6,52,53]

-- L'
leftCounterclockwise :: Cube -> Either String Cube
leftCounterclockwise cube = executeMove cube swapPositions
  where
    swapPositions = [45,1,2,48,4,5,51,7,8,9,10,11,12,13,14,15,16,17,18,19,42,21,22,39,24,25,36,29,32,35,28,31,34,27,30,33,0,37,38,3,40,41,6,43,44,26,46,47,23,49,50,20,52,53]

-- M
middleClockwise :: Cube -> Either String Cube
middleClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [0,37,2,3,40,5,6,43,8,9,10,11,12,13,14,15,16,17,26,46,24,23,49,21,20,52,18,27,28,29,30,31,32,33,34,35,36,19,38,39,22,41,42,25,44,53,1,51,50,4,48,47,7,45]

-- M'
middleCounterclockwise :: Cube -> Either String Cube
middleCounterclockwise cube = executeMove cube swapPositions
  where
    swapPositions = [0,46,2,3,49,5,6,52,8,9,10,11,12,13,14,15,16,17,26,37,24,23,40,21,20,43,18,27,28,29,30,31,32,33,34,35,36,1,38,39,4,41,42,7,44,53,19,51,50,22,48,47,25,45]

-- X
xClockwise :: Cube -> Either String Cube
xClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [45,46,47,48,49,50,51,52,53,15,12,9,16,13,10,17,14,11,36,37,38,39,40,41,42,43,44,29,32,35,28,31,34,27,30,33,0,1,2,3,4,5,6,7,8,18,19,20,21,22,23,24,25,26]

-- X'
xCounterclockwise :: Cube -> Either String Cube
xCounterclockwise cube = executeMove cube swapPositions
  where
    swapPositions = [36,37,38,39,40,41,42,43,44,11,14,17,10,13,16,9,12,15,45,46,47,48,49,50,51,52,53,33,30,27,34,31,28,35,32,29,18,19,20,21,22,23,24,25,26,0,1,2,3,4,5,6,7,8]

-- Y
yClockwise :: Cube -> Either String Cube
yClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,0,1,2,3,4,5,6,7,8,42,39,36,43,40,37,44,41,38,47,50,53,46,49,52,45,48,51]

-- Y'
yCounterclockwise :: Cube -> Either String Cube
yCounterclockwise cube = executeMove cube swapPositions
  where
    swapPositions = [27,28,29,30,31,32,33,34,35,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,38,41,44,37,40,43,36,39,42,51,48,45,52,49,46,53,50,47]

-- Z
zClockwise :: Cube -> Either String Cube
zClockwise cube = executeMove cube swapPositions
  where
    swapPositions = [6,3,0,7,4,1,8,5,2,42,39,36,43,40,37,44,41,38,20,23,26,19,22,25,18,21,24,47,50,53,46,49,52,45,48,51,29,32,35,28,31,34,27,30,33,15,12,9,16,13,10,17,14,11]

-- Z'
zCounterclockwise :: Cube -> Either String Cube
zCounterclockwise cube = executeMove cube swapPositions
  where
    swapPositions = [2,5,8,1,4,7,0,3,6,47,50,53,46,49,52,45,48,51,24,21,18,25,22,19,26,23,20,42,39,36,43,40,37,44,41,38,11,14,17,10,13,16,9,12,15,33,30,27,34,31,28,35,32,29]

