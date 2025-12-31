{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Main (main) where

import GUI.Widgets.CubeView (cubeView)
import GUI.Types

import Cube.Data
import Cube.DefaultCubes (defaultCube)

import qualified Data.Map as Map
import Data.Either
import Monomer
import Data.Text (pack)

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

