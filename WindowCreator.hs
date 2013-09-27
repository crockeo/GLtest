module WindowCreator where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Callbacks
import Config
import State
import Geom

data Config = Config String (GLsizei, GLsizei) (GLdouble, GLdouble)
data Callbacks = Callbacks (IO ()) (Maybe (IO ())) (Maybe (Key -> KeyState -> Modifiers -> Position -> IO ()))

--------------------------------
-- Default Config / Callbacks --
--------------------------------
defaultConfig :: Config
defaultConfig = Config "GLTest" (640, 480) (640, 480)

defaultCallbacks :: IO Callbacks
defaultCallbacks = do
  ps <- newIORef defaultProgramState
  return $ Callbacks (display ps)
                     (Just $ idle ps)
                     (Just $ keyMouse ps)
  where defaultProgramState = ProgramState (Square (Vector (50.0 :: GLfloat) 50.0) (Vector 50.0 50.0))
                                           (InputState False False False False)
                                           (GameConfig 'w' 's' 'a' 'd' 5.0)

----------------------------
-- Creating a GLUT Window --
----------------------------
defaultCreate :: IO ()
defaultCreate = defaultCallbacks >>= create defaultConfig

create :: Config -> Callbacks -> IO ()
create cfg cbs = do
  initGLUT   cfg cbs
  initOpenGL cfg cbs
  where initGLUT :: Config -> Callbacks -> IO ()
        initGLUT (Config title (w, h) _) (Callbacks dsp idl key) = do
          _ <- getArgsAndInitialize

          createWindow title
          windowSize $= Size w h

          displayCallback $= dsp
          initIdleIfExists   idl
          initKeyIfExists    key
          where initIdleIfExists :: (Maybe (IO ())) -> IO ()
                initIdleIfExists idl@(Just a) = idleCallback $= idl
                initIdleIfExists      Nothing = return ()

                initKeyIfExists :: (Maybe (Key -> KeyState -> Modifiers -> Position -> IO ())) -> IO ()
                initKeyIfExists key@(Just a) = keyboardMouseCallback $= key
                initKeyIfExists      Nothing = return ()


        initOpenGL :: Config -> Callbacks -> IO ()
        initOpenGL (Config _ _ (rw, rh)) cbs = do
          matrixMode $= Projection
          loadIdentity

          ortho 0 rw rh 0 (-1) 1

          matrixMode $= Modelview 2