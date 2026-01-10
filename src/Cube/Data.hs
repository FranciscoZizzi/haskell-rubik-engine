module Cube.Data (
  Cube, Sticker(..), Movement, Face(..), 
  pieces, movementsMap,
  newCube, isSolved, executeMovement, getFaceSize
) where

import qualified Data.Map as Map
import qualified Data.Bifunctor
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
} deriving Show

data Sticker = Sticker {
  colorId :: Int,
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

getFaceSize :: Cube -> Int
getFaceSize cube = 1 + getFaceSizeRec (Map.keys (pieces cube)) 0

-- Utils --

getFaceSizeRec :: [(Int, Int, Int)] -> Int -> Int
getFaceSizeRec [] acc = acc
getFaceSizeRec ((x,y,z):coords) acc = if maxVal > acc
  then getFaceSizeRec coords maxVal
  else getFaceSizeRec coords acc
  where
    maxVal = maximum [x,y,z]

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
  then updateStickers (Sticker (colorId s) (snd ori) (rotation s):acc) ss oris []
  else updateStickers (s:acc) ss (ori:oris) []
updateStickers acc (s:ss) [] (rot:rots) = if orientation s == fst rot
  then updateStickers (Sticker (colorId s) (orientation s) (snd rot):acc) ss [] rots
  else updateStickers (s:acc) ss [] (rot:rots)
updateStickers acc (s:ss) (ori:oris) (rot:rots) = case (orientation s == fst ori, orientation s == fst rot) of
  (False, False) -> updateStickers (s:acc) ss (ori:oris) (rot:rots)
  (False, True) -> updateStickers (Sticker (colorId s) (orientation s) (snd rot):acc) ss (ori:oris) rots
  (True, False) -> updateStickers (Sticker (colorId s) (snd ori) (rotation s):acc) ss oris (rot:rots)
  (True, True) -> updateStickers (Sticker (colorId s) (snd ori) (snd rot):acc) ss oris rots

isEqualPiece :: [Sticker] -> [Sticker] -> Bool
isEqualPiece p1 p2 = List.sort p1 == List.sort p2

isPartiallyEqualPiece :: [Sticker] -> [Sticker] -> Bool
isPartiallyEqualPiece p1 p2 = all (\ (s1, s2) -> (colorId s1 == colorId s2) && (orientation s1 == orientation s2)) (zip (List.sort p1) (List.sort p2))

isValidMovements :: [Movement] -> Map.Map (Int, Int, Int) [Sticker] -> Bool
isValidMovements _movementsMap _pieces = validPositions
  where
    validPositions = all (\(from, to, _, _) -> Map.member from _pieces && Map.member to _pieces) _movementsMap

instance Eq Cube where
  Cube pieces1 movementsMap1 winState1 isRotationSensitive1 == Cube pieces2 movementsMap2 winState2 isRotationSensitive2 =
    equalPieces && equalMovements && equalWinState && equalRotationSensitivity
    where
      equalPieces = 
        Map.size pieces1 == Map.size pieces2 &&
        (normalizePieces pieces1 == normalizePieces pieces2)
      equalMovements = normalizeMovements movementsMap1 == normalizeMovements movementsMap2
      equalWinState = normalizeWinState winState1 == normalizeWinState winState2
      equalRotationSensitivity = isRotationSensitive1 == isRotationSensitive2

      normalizePieces = Map.map List.sort
      normalizeMovements = Map.map (map (\(f, t, ori, rot) -> (f, t, List.sort ori, List.sort rot)))
      normalizeWinState = map (Data.Bifunctor.second List.sort)

