{-# LANGUAGE OverloadedStrings #-}
module GUI.Widgets.CoolCubeView (coolCubeWidget) where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Default
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Single
import GUI.Types

data DrawEnv = DrawEnv {
  _renderer :: Renderer,
  _width :: Double,
  _height :: Double,
  _x :: Double,
  _y :: Double
}

type Draw a = ReaderT DrawEnv IO a

coolCubeWidget :: Widget AppModel AppEvent
coolCubeWidget = createSingle () def {
  singleRender = renderCanvas
}

renderCanvas :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> Renderer -> IO ()
renderCanvas _ node renderer = do
  let viewport = node ^. L.info . L.viewport
  let (Rect x y w h) = viewport

  -- Draw Background
  drawRect renderer viewport (Just $ rgb 20 20 20) Nothing

  -- DRAWING LINES
  let env = DrawEnv renderer w h x y
  runReaderT drawScene env

drawScene :: Draw ()
drawScene = do
  renderer <- asks _renderer
  liftIO $ do
    beginPath renderer
    setStrokeColor renderer orange
    setStrokeWidth renderer 2
  
  draw3DLine (-0.5, 0.5, 1) (0.5, 0.5, 1)
  draw3DLine (-0.5, -0.5, 1) (0.5, -0.5, 1)
  draw3DLine (0.5, 0.5, 1) (0.5, -0.5, 1)
  draw3DLine (-0.5, 0.5, 1) (-0.5, -0.5, 1)

  draw3DLine (-0.5, 0.5, 2) (0.5, 0.5, 2)
  draw3DLine (-0.5, -0.5, 2) (0.5, -0.5, 2)
  draw3DLine (0.5, 0.5, 2) (0.5, -0.5, 2)
  draw3DLine (-0.5, 0.5, 2) (-0.5, -0.5, 2)

  draw3DLine (0.5, 0.5, 1) (0.5, 0.5, 2)
  draw3DLine (-0.5, 0.5, 1) (-0.5, 0.5, 2)
  draw3DLine (0.5, -0.5, 1) (0.5, -0.5, 2)
  draw3DLine (-0.5, -0.5, 1) (-0.5, -0.5, 2)
  
  liftIO $ stroke renderer

draw3DLine :: (Double, Double, Double) -> (Double, Double, Double) -> Draw ()
draw3DLine from to = do
  renderer <- asks _renderer
  w <- asks _width
  h <- asks _height
  _x <- asks _x
  _y <- asks _y

  let get2DCoordinates (x, y, z) = (x/z, y/z)
  let translateCoordinates (x, y) _width _height = (_x+(x+1)/2*_width, _y+(y+1)/2*_height)
  let getTranslatedPoint point w' h' = uncurry Point (translateCoordinates point w' h')

  liftIO $ renderLine renderer 
    (getTranslatedPoint (get2DCoordinates from) w h) 
    (getTranslatedPoint (get2DCoordinates to) w h)

