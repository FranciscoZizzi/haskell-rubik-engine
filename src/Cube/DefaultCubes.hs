module Cube.DefaultCubes (defaultCube) where

import Cube.Data
import qualified Data.Map as Map

defaultCube :: Cube
defaultCube = case newCube defaultPieces defaultMovements False of
  Right cube -> cube
  Left _ -> error "This should not happen"
  where
    defaultPieces = Map.fromList [
      ((0,0,0), [Sticker 1 D 0, Sticker 2 F 0, Sticker 5 L 0]),
      ((1,0,0), [Sticker 1 D 0, Sticker 2 F 0]),
      ((2,0,0), [Sticker 2 F 0, Sticker 1 D 0, Sticker 3 R 0]),
      ((0,1,0), [Sticker 5 L 0, Sticker 1 D 0]),
      ((1,1,0), [Sticker 1 D 0]),
      ((2,1,0), [Sticker 3 R 0, Sticker 1 D 0]),
      ((0,2,0), [Sticker 5 L 0, Sticker 1 D 0, Sticker 4 B 0]),
      ((1,2,0), [Sticker 4 B 0, Sticker 1 D 0]),
      ((2,2,0), [Sticker 3 R 0, Sticker 1 D 0, Sticker 4 B 0]),

      ((0,0,1), [Sticker 5 L 0, Sticker 2 F 0]),
      ((1,0,1), [Sticker 2 F 0]),
      ((2,0,1), [Sticker 2 F 0, Sticker 3 R 0]),
      ((0,1,1), [Sticker 5 L 0]),
      ((2,1,1), [Sticker 3 R 0]),
      ((0,2,1), [Sticker 5 L 0, Sticker 4 B 0]),
      ((1,2,1), [Sticker 4 B 0]),
      ((2,2,1), [Sticker 4 B 0, Sticker 3 R 0]),

      ((0,0,2), [Sticker 0 U 0, Sticker 2 F 0, Sticker 5 L 0]),
      ((1,0,2), [Sticker 0 U 0, Sticker 2 F 0]),
      ((2,0,2), [Sticker 2 F 0, Sticker 0 U 0, Sticker 3 R 0]),
      ((0,1,2), [Sticker 5 L 0, Sticker 0 U 0]),
      ((1,1,2), [Sticker 0 U 0]),
      ((2,1,2), [Sticker 3 R 0, Sticker 0 U 0]),
      ((0,2,2), [Sticker 5 L 0, Sticker 0 U 0, Sticker 4 B 0]),
      ((1,2,2), [Sticker 4 B 0, Sticker 0 U 0]),
      ((2,2,2), [Sticker 3 R 0, Sticker 0 U 0, Sticker 4 B 0])]

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
        ]),
      ("U", [
        ((0,0,2), (0,2,2), [(L, B), (F, L)], []),
        ((1,0,2), (0,1,2), [(F, L)], []),
        ((2,0,2), (0,0,2), [(F, L), (R, F)], []),
        ((0,1,2), (1,2,2), [(L, B)], []),
        ((1,1,2), (1,1,2), [], []),
        ((2,1,2), (1,0,2), [(R, F)], []),
        ((0,2,2), (2,2,2), [(L, B), (B, R)], []),
        ((1,2,2), (2,1,2), [(B, R)], []),
        ((2,2,2), (2,0,2), [(B, R), (R, F)], [])
        ])]

