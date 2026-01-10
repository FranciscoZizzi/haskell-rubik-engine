{-# LANGUAGE OverloadedStrings #-}
module GUI.Widgets.CoolCubeView (coolCubeWidget) where

import Debug.Trace

import Control.Lens ((^.))
import Control.Monad.Reader
import Control.Monad
import Data.Default
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Single
import GUI.Types
import TextShow (printT)

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
   
  let distanceFromScreen = 1.5 -- The distance from the center of the cube to the screen (z=0)
  let sizeStickers = 3
  let xzRotation = -pi/4
  let yzRotation = pi/6
  let xyRotation = 0

  drawCube distanceFromScreen (xyRotation, xzRotation, yzRotation) sizeStickers

  liftIO $ stroke renderer

drawCube :: Double -> (Double, Double, Double) -> Int -> Draw ()
drawCube distance (xyRotation, xzRotation, yzRotation) sizeStickers = do
  drawFace (getLeftFace points) green
  drawFace (getFrontFace points) red
  drawFace (getTopFace points) white
  drawFace (getRightFace points sizeStickers) blue
  drawFace (getBottomFace points sizeStickers) yellow
  drawFace (getBackFace points sizeStickers) orange
  where
    cornerPos = 0.5
    points = [[[rotatePoint (((cornerPos,cornerPos,cornerPos) `vectorProduct` normalizePoint (fromIntegral i,fromIntegral j,fromIntegral k)) `vectorAddition` (0,0,distance))
      | i <- [0..sizeStickers]]
        | j <- [0..sizeStickers]]
          | k <- [0..sizeStickers]]
    s = fromIntegral sizeStickers
    normalizePoint (x, y, z) = ((2*x)/s - 1, (2*y)/s - 1, (2*z)/s - 1)
    rotatePoint point = rotateXY (rotateYZ (rotateXZ point xzRotation distance) yzRotation distance) xyRotation

getFrontFace :: [[[(Double, Double, Double)]]] -> [[(Double, Double, Double)]]
getFrontFace grid = grid !! 0

getBackFace :: [[[(Double, Double, Double)]]] -> Int -> [[(Double, Double, Double)]]
getBackFace grid size = reverse $ grid !! size

getTopFace :: [[[(Double, Double, Double)]]] -> [[(Double, Double, Double)]]
getTopFace grid = reverse $ map (!! 0) grid

getBottomFace :: [[[(Double, Double, Double)]]] -> Int -> [[(Double, Double, Double)]]
getBottomFace grid size = map (!! size) grid

getLeftFace :: [[[(Double, Double, Double)]]] -> [[(Double, Double, Double)]]
getLeftFace grid = map (map (!! 0)) grid

getRightFace :: [[[(Double, Double, Double)]]] -> Int -> [[(Double, Double, Double)]]
getRightFace grid size = reverse $ map (map (!! size)) grid

drawFace :: [[(Double, Double, Double)]] -> Color -> Draw ()
drawFace face color = do
  let size = length face - 1
  forM_ [0..size-1] $ \r ->
    forM_ [0..size-1] $ \c -> do
      -- Define the 4 corners of a single sticker
      let p1 = face !! r !! c
      let p2 = face !! (r+1) !! c
      let p3 = face !! (r+1) !! (c+1)
      let p4 = face !! r !! (c+1)
      
      -- Project and Fill (See note below on lineTo)
      drawPolygon (p1, p2, p3, p4) color

drawPolygon :: ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double), (Double, Double, Double)) -> Color-> Draw ()
drawPolygon (p1, p2, p3, p4) color = do
  renderer <- asks _renderer
  w <- asks _width
  h <- asks _height
  xOffset <- asks _x
  yOffset <- asks _y

  let projectPoint p = project p xOffset yOffset w h
  let p1' = projectPoint p1
  let p2' = projectPoint p2
  let p3' = projectPoint p3
  let p4' = projectPoint p4

  if isVisibleFace p1 p2 p3 
  then liftIO $ do
    setStrokeColor renderer color
    setStrokeWidth renderer 1
    drawTriangle renderer p1' p2' p3' (Just color)
    drawTriangle renderer p1' p3' p4' (Just color)
  else liftIO $ do
    setStrokeWidth renderer 1 -- idk how to make it do nothing hehe :-)
  where 
    project (x, y, z) xOffset yOffset w h =
      Point (xOffset + (x'+1)/2*w) (yOffset + (y'+1)/2*h)
        where
          x' = x/z
          y' = y/z

isVisibleFace :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> Bool
isVisibleFace p1 p2 p3 = (n `dotProduct` p1) < 0
  where
    a = p2 `vectorSubtraction` p1
    b = p3 `vectorSubtraction` p1
    n = a `crossProduct` b

-- draw3DLine :: (Double, Double, Double) -> (Double, Double, Double) -> Draw ()
-- draw3DLine from to = do
--   renderer <- asks _renderer
--   w <- asks _width
--   h <- asks _height
--   _x <- asks _x
--   _y <- asks _y
--
--   let get2DCoordinates (x, y, z) = (x/z, y/z)
--   let translateCoordinates (x, y) _width _height = (_x+(x+1)/2*_width, _y+(y+1)/2*_height)
--   let getTranslatedPoint point w' h' = uncurry Point (translateCoordinates point w' h')
--
--   liftIO $ renderLine renderer 
--     (getTranslatedPoint (get2DCoordinates from) w h) 
--     (getTranslatedPoint (get2DCoordinates to) w h)
--
rotateXZ :: (Double, Double, Double) -> Double -> Double -> (Double, Double, Double)
rotateXZ (x, y, z) angle distance = 
  (x*c - z'*s, 
  y, 
  x*s + z'*c + distance)
  where
    z'= z-distance
    s = sin angle
    c = cos angle

rotateYZ :: (Double, Double, Double) -> Double -> Double -> (Double, Double, Double)
rotateYZ (x, y, z) angle distance = 
  (x,
  y*c - z'*s, 
  y*s + z'*c + distance)
  where
    z'= z-distance
    s = sin angle
    c = cos angle

-- This one does not require distance because the cube center will always be at x=0 y=0
rotateXY :: (Double, Double, Double) -> Double -> (Double, Double, Double)
rotateXY (x, y, z) angle = 
  (x*c - y*s,
  x*s + y*c, 
  z)
  where
    s = sin angle
    c = cos angle

crossProduct :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
crossProduct (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

vectorProduct :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
vectorProduct (a1, a2, a3) (b1, b2, b3) = (a1 * b1, a2 * b2, a3 * b3)

vectorAddition :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
vectorAddition (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

vectorSubtraction :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
vectorSubtraction (a1, a2, a3) (b1, b2, b3) = (a1 - b1, a2 - b2, a3 - b3)

dotProduct :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dotProduct (a1, a2, a3) (b1, b2, b3) = a1*b1 + a2*b2 + a3*b3
