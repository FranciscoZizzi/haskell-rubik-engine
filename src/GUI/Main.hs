{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GUI.Main (main) where

import GUI.Widgets.FlatCubeView (flatCubeView)
import GUI.Widgets.CoolCubeView
import GUI.Types

import Cube.Data
import Cube.DefaultCubes (defaultCube)

import qualified Data.Map as Map
import Data.Either
import Monomer
import Data.Text (pack)
import Control.Lens

getMovementButtons :: Cube -> [WidgetNode AppModel AppEvent]
getMovementButtons c = buttons
  where
    buttons = map (\move -> button (pack move) (ApplyMove move)) (Map.keys (movementsMap c))

buildUI :: 
  WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = mainPage
  where
    mainPage = hstack [
      vstack (
        getMovementButtons (model ^. cube)++[
        spacer,
        flatCubeView (model ^. cube) (model ^. colorMap) 150
        ]),
      spacer,
      vstack [
        defaultWidgetNode "lines" coolCubeWidget `styleBasic` [width 400, height 400],
        spacer,
        hslider yzRotation 0 360 `styleBasic` [height 10, width 400]
      ],
      spacer,
      vstack [
        vslider xzRotation 0 360 `styleBasic` [height 400]
      ]
      ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  ApplyMove move -> [Model $ model & cube .~ fromRight defaultCube (executeMovement (model ^. cube) move)]

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
    model = AppModel defaultCube colorMap_ 0 0
    colorMap_ = Map.fromList [(0, white), (1, yellow), (2, red), (3, blue), (4, orange), (5, green)]

