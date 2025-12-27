module Cube.Data (
  Cube, Sticker(..), Movement, Face(..), 
  pieces, movementsMap,
  newCube, isSolved, executeMovement
) where

import qualified Data.Map as Map
import qualified Data.List as List

type Movement = (
  (Int, Int, Int), -- Initial position
  (Int, Int, Int), -- Final position
  [(Face, Face)],  -- From - To
  [(Face, Int)])   -- Final sticker rotations

data Cube = Cube {
  pieces :: Map.Map (Int, Int, Int) [Sticker],
  movementsMap :: Map.Map String [Movement],
  winState :: [((Int, Int, Int), [Sticker])],
  isRotationSensitive :: Bool -- Determines if the solved state takes in consideration the sticker rotations
} deriving (Show, Eq)

data Sticker = Sticker {
  stickerId :: Int,
  orientation:: Face,
  rotation :: Int
} deriving (Show, Eq, Ord)

data Face = F | B | L | R | D | U deriving (Show, Eq, Ord, Enum)

newCube :: Map.Map (Int, Int, Int) [Sticker] -> Map.Map String [Movement] -> Bool -> Either String Cube
newCube _pieces _movementsMap _isRotationSensitive = if all (`isValidMovements` _pieces) (Map.elems _movementsMap)
  then Right (Cube _pieces _movementsMap _winState _isRotationSensitive)
  else Left "Invalid Movements"
  where
    _winState = Map.assocs _pieces

executeMovement :: Cube -> String -> Either String Cube
executeMovement cube movementName = case movementResult of 
  Nothing -> Left ("Invalid movement: "++movementName)
  Just movementSteps -> Right (Cube (newPieces movementSteps) (movementsMap cube) (winState cube) (isRotationSensitive cube))
  where
    cubePieces = pieces cube
    movementResult = Map.lookup movementName (movementsMap cube)
    newPieces = List.foldl' updatePieceAndInsert cubePieces
    updatePieceAndInsert acc (initialPos, finalPos, newOrientation, newRotation) = 
      let pieceToUpdate = cubePieces Map.! initialPos
          updatedPiece = updatePiece pieceToUpdate newOrientation newRotation
      in Map.insert finalPos updatedPiece acc

isSolved :: Cube -> Bool
isSolved cube = all (\(position, piece) -> compareFunction piece (cubePieces Map.! position)) cubeWinState
  where
    compareFunction = if isRotationSensitive cube then isEqualPiece else isPartiallyEqualPiece
    cubePieces = pieces cube
    cubeWinState = winState cube

-- Utils --

updatePiece :: [Sticker] -> [(Face, Face)] -> [(Face, Int)] -> [Sticker]
updatePiece stickers newOrientations newRotations = updateStickers [] sortedStickers sortedOrientations sortedRotations
  where
    sortedStickers = List.sortOn orientation stickers
    sortedOrientations = List.sort newOrientations
    sortedRotations = List.sort newRotations

updateStickers :: [Sticker] -> [Sticker] -> [(Face, Face)] -> [(Face, Int)] -> [Sticker]
updateStickers acc [] _ _ = acc
updateStickers acc s [] [] = acc++s
updateStickers acc (s:ss) (ori:oris) [] = if orientation s == fst ori
  then updateStickers (Sticker (stickerId s) (snd ori) (rotation s):acc) ss oris []
  else updateStickers (s:acc) ss (ori:oris) []
updateStickers acc (s:ss) [] (rot:rots) = if orientation s == fst rot
  then updateStickers (Sticker (stickerId s) (orientation s) (snd rot):acc) ss [] rots
  else updateStickers (s:acc) ss [] (rot:rots)
updateStickers acc (s:ss) (ori:oris) (rot:rots) = case (orientation s == fst ori, orientation s == fst rot) of
  (False, False) -> updateStickers (s:acc) ss (ori:oris) (rot:rots)
  (False, True) -> updateStickers (Sticker (stickerId s) (orientation s) (snd rot):acc) ss (ori:oris) rots
  (True, False) -> updateStickers (Sticker (stickerId s) (snd ori) (rotation s):acc) ss oris (rot:rots)
  (True, True) -> updateStickers (Sticker (stickerId s) (snd ori) (snd rot):acc) ss oris rots

isEqualPiece :: [Sticker] -> [Sticker] -> Bool
isEqualPiece p1 p2 = List.sort p1 == List.sort p2

isPartiallyEqualPiece :: [Sticker] -> [Sticker] -> Bool
isPartiallyEqualPiece p1 p2 = all (\ (s1, s2) -> (stickerId s1 == stickerId s2) && (orientation s1 == orientation s2)) (zip (List.sort p1) (List.sort p2))

isValidMovements :: [Movement] -> Map.Map (Int, Int, Int) [Sticker] -> Bool
isValidMovements _movementsMap _pieces = validPositions
  where
    validPositions = all (\(from, to, _, _) -> Map.member from _pieces && Map.member to _pieces) _movementsMap
