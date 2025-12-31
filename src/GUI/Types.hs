{-# LANGUAGE TemplateHaskell #-}
module GUI.Types where

import Cube.Data
import qualified Data.Map as Map
import Control.Lens.TH
import Monomer

data AppModel = AppModel {
  cube :: Cube,
  colorMap :: Map.Map Int Color
} deriving (Eq, Show)

data AppEvent = 
  AppInit | 
  ApplyMove String
  deriving (Eq, Show)

makeLenses 'AppModel
