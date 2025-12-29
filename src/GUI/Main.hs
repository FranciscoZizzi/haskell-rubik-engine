module GUI.Main (main) where

import GUI.Types
import Data.Text (Text)
import Text.Show
import Monomer
import Control.Lens

import qualified Monomer.Lens as L

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
    model = AppModel 0

buildUI :: 
  WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree = vstack [
      label "Hello world",
      spacer,
      hstack [
        label "Hello",
        spacer,
        label "World",
        spacer,
        button "Click me!" AppIncrease
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
  AppIncrease -> [Model (model & clickCount +~ 1)]

