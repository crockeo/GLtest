module Geom where

import Graphics.Rendering.OpenGL

data Vector = Vector GLfloat GLfloat
  deriving (Eq, Show, Read)

data Square = Square Vector Vector
  deriving (Eq, Show, Read)

-- Binding a vector
bindVector :: Vector -> IO ()
bindVector (Vector x y) =
  vertex $ Vertex2 x y

-- Generating the vertecies of a square
generateRenderPoints :: Square -> [Vector]
generateRenderPoints (Square (Vector x y) (Vector w h)) =
  [Vector (x) (y), Vector (x + w) (y), Vector (x + w) (y + h), Vector (x) (y + h)]