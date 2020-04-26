module GUI where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Array
import Data.Time.Clock
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Linear
import System.Exit
import System.FSNotify

import Life
import OpenGL
import STMState

data VisualisationState = VisualisationState
  { _simSpeed :: Int
  }
  deriving Show
makeLenses ''VisualisationState

mainOpenGL :: FilePath -> IO ()
mainOpenGL shaderDir = do
  makeWindow
  initOGL

  rs <- initRenderState shaderDir
  let w = exampleWorld 20 10
  let vs = VisualisationState 0
  rsTVar <- liftIO (newTVarIO rs)
  wTVar <- liftIO (newTVarIO w)
  vsTVar <- liftIO (newTVarIO vs)

  recompileOnChange rsTVar shaderDir
  postInit rsTVar
  GLFW.keyCallback $= keyCallback vsTVar

  t0 <- liftIO (getCurrentTime)
  void $ runSTMStateT vsTVar $ runSTMStateT wTVar $ runSTMStateT rsTVar $ do
    forever $ do
      w <- get @World
      vs <- get @VisualisationState
      (_, t) <- elapsedTime t0
      renderFrame t (worldToVertices w)
      modify (((!! (2^(vs^.simSpeed)))) . iterate simulate)
      liftIO (threadDelay 5000)

keyCallback :: TVar VisualisationState -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
keyCallback _ (GLFW.SpecialKey GLFW.ESC) GLFW.Press = exitSuccess
keyCallback s (GLFW.CharKey '.') GLFW.Press
  = void $ runSTMStateT s (modify (over simSpeed (+1)))
keyCallback s (GLFW.CharKey ',') GLFW.Press
  = void $ runSTMStateT s (modify (over simSpeed (max 0 . (subtract 1))))
keyCallback _ _ _ = pure ()

colourPoint :: Point -> GL.Vector3 Float
colourPoint p = case p^.cell of
  Nothing -> GL.Vector3 (x / 100) 0 1
  Just c -> GL.Vector3
    (realToFrac (c^.dna.membraneDiffusion))
    (realToFrac (c^.dna.properties.key Photosynthesiser) / 4)
    0
  where x = realToFrac (p^.quantities.key Energy)

worldToVertices :: World -> [Vertex]
worldToVertices w
  = [ (GL.Vector2 (fromIntegral x') (fromIntegral y'), colourPoint ((w^.grid)!v))
    | x' <- [0..w^.width-1], y' <- [0..w^.height-1], let v = x'*^x + y'*^y]

setDirty :: MonadIO m => TVar RenderState -> m ()
setDirty s = liftIO $ atomically $ do
  rs <- readTVar s
  writeTVar s (set dirty True rs)

elapsedTime :: MonadIO m => UTCTime -> m (Float, UTCTime)
elapsedTime t0 = do
  t <- liftIO getCurrentTime
  let iTime = diffUTCTime t t0
  pure (realToFrac iTime, t)

recompileOnChange :: MonadIO m => TVar RenderState -> FilePath -> m ()
recompileOnChange s shaderDir = liftIO $ do
  mgr <- startManager
  void $ watchDir mgr shaderDir (const True) (const (setDirty s))

