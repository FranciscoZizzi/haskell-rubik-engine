{-# LANGUAGE TemplateHaskell #-}
module GUI.Types where

import Cube.Data
import Control.Lens.TH

data AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent = 
  AppInit | 
  AppIncrease
  deriving (Eq, Show)

makeLenses 'AppModel
