module Callbacks where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Monad  

import Data.IORef

import Config
import State
import Geom

----------------------
-- Display Callback --
----------------------
display :: IORef ProgramState -> IO ()
display psRef = do
  clear [ColorBuffer]

  (readIORef psRef) >>=
    (\(ProgramState sqr _ _) ->
      renderPrimitive Quads $ do
        color $ Color3 (1.0 :: GLfloat) 1.0 1.0
        mapM_ (bindVector) $ generateRenderPoints sqr)

  flush
  postRedisplay Nothing

-------------------
-- Idle Callback --
-------------------
idle :: IORef ProgramState -> IO ()
idle psRef = do
  ps@(ProgramState sqr is gc@(GameConfig _ _ _ _ speed)) <- readIORef psRef

  writeIORef psRef $ ProgramState (updateSquare sqr is speed) is gc
  where updateSquare :: Square -> InputState -> GLfloat -> Square
        updateSquare s@(Square (Vector x y) size) (InputState up down left right) speed
          | up        = updateSquare (Square (Vector  x         (y - speed)) size) (InputState False down  left  right) speed
          | down      = updateSquare (Square (Vector  x         (y + speed)) size) (InputState up    False left  right) speed
          | left      = updateSquare (Square (Vector (x - speed) y)          size) (InputState up    down  False right) speed
          | right     = updateSquare (Square (Vector (x + speed) y)          size) (InputState up    down  left  False) speed
          | otherwise = s 

-------------------------------
-- Keyboard & Mouse Callback --
-------------------------------
keyMouse :: IORef ProgramState -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyMouse psRef k ks m p = do
  ps <- readIORef psRef
  writeIORef psRef $ handleKeys k ks ps
  where handleKeys :: Key -> KeyState -> ProgramState -> ProgramState
        handleKeys (Char c) ks ps@(ProgramState s (InputState up down left right) gc@(GameConfig upk downk leftk rightk _))
          | c == upk    = ProgramState s (InputState (toBool ks) down        left       right     ) gc
          | c == downk  = ProgramState s (InputState  up        (toBool ks)  left       right     ) gc
          | c == leftk  = ProgramState s (InputState  up         down       (toBool ks) right     ) gc
          | c == rightk = ProgramState s (InputState  up         down        left      (toBool ks)) gc
          | otherwise   = ps
        handleKeys k ks ps = ps