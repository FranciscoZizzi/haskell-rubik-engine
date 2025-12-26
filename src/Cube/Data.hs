module Cube.Data (
  Cube, Piece, Sticker, Movement,
  pieces, movementsMap, stickers, orientation, stickerId, rotation,
  newCube, isSolved, executeMovement
) where

import qualified Data.Map as Map
import qualified Data.List as List

type Movement = (
  (Int, Int, Int),  -- Initial position
  (Int, Int, Int),  -- Final position
  [Int],            -- Final orientation
  [Int])            -- Final sticker rotations

data Cube = Cube {
  pieces :: Map.Map (Int, Int, Int) Piece,
  movementsMap :: Map.Map String [Movement],
  winState :: [((Int, Int, Int), Piece)],
  isRotationSensitive :: Bool -- Determines if the solved state takes in consideration the sticker rotations
}

data Piece = Piece {
  stickers :: [Sticker],
  orientation :: [Int]
} deriving Eq

data Sticker = Sticker {
  stickerId :: Int,
  rotation :: Int
} deriving Eq

newCube :: Map.Map (Int, Int, Int) Piece -> Map.Map String [Movement] -> Bool -> Either String Cube
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
      let pieceToUpdate = acc Map.! initialPos
          updatedPiece = updatePiece pieceToUpdate newOrientation newRotation
      in Map.insert finalPos updatedPiece acc

isSolved :: Cube -> Bool
isSolved cube = all (\(position, piece) -> compareFunction piece (cubePieces Map.! position)) cubeWinState
  where
    compareFunction = if isRotationSensitive cube then isEqualPiece else isPartiallyEqualPiece
    cubePieces = pieces cube
    cubeWinState = winState cube

-- Utils
updatePiece :: Piece -> [Int] -> [Int] -> Piece
updatePiece piece newOrientation newRotations = Piece newStickers newOrientation
  where 
     newStickers = zipWith (Sticker . stickerId) (stickers piece) newRotations

isEqualPiece :: Piece -> Piece -> Bool
isEqualPiece p1 p2 = p1 == p2

isPartiallyEqualPiece :: Piece -> Piece -> Bool
isPartiallyEqualPiece p1 p2 = equalOrientations && equalStickerIds
  where
    equalOrientations = orientation p1 == orientation p2
    equalStickerIds = getStickerIds p1 == getStickerIds p2
    getStickerIds piece = map stickerId (stickers piece)

isValidMovements :: [Movement] -> Map.Map (Int, Int, Int) Piece -> Bool
isValidMovements _movementsMap _pieces = validPositions
  where
    validPositions = all (\(from, to, _, _) -> Map.member from _pieces && Map.member to _pieces) _movementsMap
