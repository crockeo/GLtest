module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data Block = Block (GLfloat, GLfloat) (GLfloat, GLfloat)
  deriving (Eq, Show, Read)

block :: Block
block = Block ((0.0 :: GLfloat), 0.0) (0.5, 0.5)

renderBlock :: Block -> IO ()
renderBlock (Block (x, y) (w, h)) =
  renderPrimitive Quads $ do
    color $ Color3 (1.0 :: GLfloat) 1.0 1.0
    vertex $ Vertex2 (x) (y)
    vertex $ Vertex2 (x + w) (y)
    vertex $ Vertex2 (x + w) (y + h)
    vertex $ Vertex2 (x) (y + h)

-- The display function
display :: IO ()
display = do
  clear [ColorBuffer]

  renderBlock block

  flush

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  createWindow "Test"

  displayCallback $= display

  mainLoop