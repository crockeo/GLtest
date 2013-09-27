module State where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Config
import Geom

data InputState = InputState Bool Bool Bool Bool
  deriving (Eq, Show, Read)

toBool :: KeyState -> Bool
toBool Up   = False
toBool Down = True

data ProgramState = ProgramState Square InputState GameConfig
  deriving (Eq, Show, Read)