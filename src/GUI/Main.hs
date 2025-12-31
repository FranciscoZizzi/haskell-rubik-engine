{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Main (main) where

import GUI.Types
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Data.Either
import Monomer
import Data.Text (pack)

import Cube.Data
import Cube.DefaultCubes (defaultCube)

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

getMovementButtons :: Cube -> WidgetNode AppModel AppEvent
getMovementButtons c = vstack buttons
  where
    buttons = map (\move -> button (pack move) (ApplyMove move)) (Map.keys (movementsMap c))

buildUI :: 
  WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree = hstack [
      cubeView (cube model) (colorMap model),
      getMovementButtons (cube model)
      ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  ApplyMove move -> [Model (AppModel (fromRight defaultCube (executeMovement (cube model) move)) (colorMap model))]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Cube Thingy",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel defaultCube colorMap_
    colorMap_ = Map.fromList [(0, yellow), (1, red), (2, green), (3, yellow), (4, red), (5, red), (6, yellow), (7, blue), (8, green), (9, yellow), (10, yellow), (11, blue), (12, yellow), (13, green), (14, yellow), (15, orange), (16, orange), (17, yellow), (18, blue), (19, yellow), (20, orange), (21, green), (22, red), (23, red), (24, red), (25, blue), (53, green), (26, blue), (27, green), (28, orange), (29, orange), (30, orange), (31, blue), (32, white), (33, red), (34, green), (35, white), (36, red), (37, red), (38, white), (39, blue), (40, green), (41, white), (42, white), (43, blue), (44, white), (45, green), (46, white), (47, orange), (48, orange), (49, white), (50, blue), (51, white), (52, orange)]

