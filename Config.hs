module Config where

import Graphics.Rendering.OpenGL

data GameConfig = GameConfig Char Char Char Char GLfloat
  deriving (Eq, Show, Read)