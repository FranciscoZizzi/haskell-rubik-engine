module GUI.Widgets.CubeView (cubeView) where

import Cube.Data

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Monomer

sticker :: (WidgetModel s, WidgetEvent e) => Double -> Color -> WidgetNode s e
sticker stickerSize color = box (box (label "")
  `styleBasic` [
    width actualStickerSize,
    height actualStickerSize,
    bgColor color
  ]) `styleBasic` [padding stickerPadding]
  where
    actualStickerSize = stickerSize * 0.90
    stickerPadding = stickerSize * 0.10 / 2

cubeFace :: (WidgetModel s, WidgetEvent e) => [[Color]] -> Double -> WidgetNode s e
cubeFace colors stickerSize = box (
  vgrid stickerGrids)
  where
    stickerGrids = map (hgrid . map (sticker stickerSize)) colors

cubeView :: (WidgetModel s, WidgetEvent e) => Cube -> Map.Map Int Color -> Double -> WidgetNode s e
cubeView c colorMap_ cubeWidth = box (
  hgrid [
      vgrid [cubeFace (getFaceColors L) stickerWidth],
      vgrid [
        cubeFace (getFaceColors U) stickerWidth,
        cubeFace (getFaceColors F) stickerWidth,
        cubeFace (getFaceColors D) stickerWidth
        ],
      vgrid [cubeFace (getFaceColors R) stickerWidth],
      vgrid [cubeFace (getFaceColors B) stickerWidth]
    ]
  )
  where
    getFaceColors face = [[getStickerColor $ getFaceSticker face $ getPiece (x,y) face | x <- [0..faceSize]] | y <- [0..faceSize]]
    faceSize = getFaceSize c
    getPiece coords face = pieces c Map.! transform2dto3d coords face faceSize
    getFaceSticker face = List.find (\s -> face == orientation s)
    getStickerColor maybeSticker = case maybeSticker of
      Nothing -> black 
      Just s -> fromMaybe black (Map.lookup (colorId s) colorMap_)
    stickerWidth = cubeWidth / fromIntegral (faceSize * 4)

transform2dto3d :: (Int, Int) -> Face -> Int -> (Int, Int, Int)
transform2dto3d (x, y) face faceSize = case face of
  L -> (0, faceSize-x, faceSize-y)
  U -> (x, faceSize-y, 2)
  F -> (x, 0, faceSize-y)
  D -> (x, y, 0)
  R -> (2, x, faceSize-y)
  B -> (faceSize-x, 2, faceSize-y)


