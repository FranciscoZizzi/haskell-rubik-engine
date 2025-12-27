module Cube.DefaultCubes (defaultCube) where

import Cube.Data
import qualified Data.Map as Map

defaultCube :: Cube
defaultCube = case newCube defaultPieces defaultMovements False of
  Right cube -> cube
  Left _ -> error "This should not happen"
  where
    defaultPieces = Map.fromList [
      ((0,0,0), [Sticker 0 D 0, Sticker 1 F 0, Sticker 2 L 0]),
      ((1,0,0), [Sticker 3 D 0, Sticker 4 F 0]),
      ((2,0,0), [Sticker 5 F 0, Sticker 6 D 0, Sticker 7 R 0]),
      ((0,1,0), [Sticker 8 L 0, Sticker 9 D 0]),
      ((1,1,0), [Sticker 10 D 0]),
      ((2,1,0), [Sticker 11 R 0, Sticker 12 D 0]),
      ((0,2,0), [Sticker 13 L 0, Sticker 14 D 0, Sticker 15 B 0]),
      ((1,2,0), [Sticker 16 B 0, Sticker 17 D 0]),
      ((2,2,0), [Sticker 18 R 0, Sticker 19 D 0, Sticker 20 B 0]),

      ((0,0,1), [Sticker 21 L 0, Sticker 22 F 0]),
      ((1,0,1), [Sticker 23 F 0]),
      ((2,0,1), [Sticker 24 F 0, Sticker 25 R 0]),
      ((0,1,1), [Sticker 53 L 0]),
      ((2,1,1), [Sticker 26 R 0]),
      ((0,2,1), [Sticker 27 L 0, Sticker 28 B 0]),
      ((1,2,1), [Sticker 29 B 0]),
      ((2,2,1), [Sticker 30 B 0, Sticker 31 R 0]),

      ((0,0,2), [Sticker 32 U 0, Sticker 33 F 0, Sticker 34 L 0]),
      ((1,0,2), [Sticker 35 U 0, Sticker 36 F 0]),
      ((2,0,2), [Sticker 37 F 0, Sticker 38 U 0, Sticker 39 R 0]),
      ((0,1,2), [Sticker 40 L 0, Sticker 41 U 0]),
      ((1,1,2), [Sticker 42 U 0]),
      ((2,1,2), [Sticker 43 R 0, Sticker 44 U 0]),
      ((0,2,2), [Sticker 45 L 0, Sticker 46 U 0, Sticker 47 B 0]),
      ((1,2,2), [Sticker 48 B 0, Sticker 49 U 0]),
      ((2,2,2), [Sticker 50 R 0, Sticker 51 U 0, Sticker 52 B 0])]

    defaultMovements = Map.fromList [
      ("F", [
        ((0,0,0), (0,0,2), [(L, U), (D, L)], []),
        ((1,0,0), (0,0,1), [(D, L)], []),
        ((2,0,0), (0,0,0), [(D, L), (R, D)], []),
        ((0,0,1), (1,0,2), [(L, U)], []),
        ((1,0,1), (1,0,1), [], []),
        ((2,0,1), (1,0,0), [(R, D)], []),
        ((0,0,2), (2,0,2), [(L, U), (U, R)], []),
        ((1,0,2), (2,0,1), [(U, R)], []),
        ((2,0,2), (2,0,0), [(U, R), (R, D)], [])
        ])]

