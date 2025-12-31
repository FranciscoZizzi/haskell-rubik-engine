module GUI.Widgets.CubeView (cubeView) where

import Cube.Data

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Monomer

sticker :: (WidgetModel s, WidgetEvent e) => Color -> WidgetNode s e
sticker color = box (box (label "")
  `styleBasic` [
    width 45,
    height 45,
    bgColor color
  ]) `styleBasic` [padding 2.5]

cubeFace :: (WidgetModel s, WidgetEvent e) => [[Color]] -> WidgetNode s e
cubeFace colors = box (
  vgrid stickerGrids)
  where
    stickerGrids = map (hgrid . map sticker) colors

cubeView :: (WidgetModel s, WidgetEvent e) => Cube -> Map.Map Int Color -> WidgetNode s e
cubeView c colorMap_ = box (
  hgrid [
      vgrid [cubeFace lFace],
      vgrid [
        cubeFace uFace,
        cubeFace fFace,
        cubeFace dFace
        ],
      vgrid [cubeFace rFace],
      vgrid [cubeFace bFace]
    ]
  )
  where
    fFace = getFaceColors F
    uFace = getFaceColors U
    dFace = getFaceColors D
    lFace = getFaceColors L
    rFace = getFaceColors R
    bFace = getFaceColors B
    getFaceColors face = [[getStickerColor $ getFaceSticker face $ getPiece (x,y) face | x <- [0..faceSize]] | y <- [0..faceSize]]
    faceSize = getFaceSize c
    getPiece coords face = pieces c Map.! transform2dto3d coords face faceSize
    getFaceSticker face = List.find (\s -> face == orientation s)
    getStickerColor maybeSticker = case maybeSticker of
      Nothing -> black 
      Just s -> fromMaybe black (Map.lookup (stickerId s) colorMap_)

transform2dto3d :: (Int, Int) -> Face -> Int -> (Int, Int, Int)
transform2dto3d (x, y) face faceSize = case face of
  L -> (0, faceSize-x, faceSize-y)
  U -> (x, faceSize-y, 2)
  F -> (x, 0, faceSize-y)
  D -> (x, y, 0)
  R -> (2, x, faceSize-y)
  B -> (faceSize-x, 2, faceSize-y)


