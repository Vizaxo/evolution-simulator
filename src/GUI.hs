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
  , _quantity :: Quantity
  , _showCells :: Bool
  }
  deriving Show
makeLenses ''VisualisationState

mainOpenGL :: FilePath -> IO ()
mainOpenGL shaderDir = do
  makeWindow
  initOGL

  rs <- initRenderState shaderDir
  let w = exampleWorld 20 10
  let vs = VisualisationState 0 Energy True
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
      renderFrame t (worldToVertices vs w)
      modify (((!! (2^(vs^.simSpeed)))) . iterate simulate)
      liftIO (threadDelay 5000)

keyCallback :: TVar VisualisationState -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
keyCallback _ (GLFW.SpecialKey GLFW.ESC) GLFW.Press = exitSuccess
keyCallback s (GLFW.CharKey '.') GLFW.Press
  = void $ runSTMStateT s (modify (over simSpeed (+1)))
keyCallback s (GLFW.CharKey ',') GLFW.Press
  = void $ runSTMStateT s (modify (over simSpeed (max 0 . (subtract 1))))
keyCallback s (GLFW.CharKey 'E') GLFW.Press
  = void $ runSTMStateT s (modify (set quantity Energy))
keyCallback s (GLFW.CharKey 'A') GLFW.Press
  = void $ runSTMStateT s (modify (set quantity A))
keyCallback s (GLFW.CharKey 'B') GLFW.Press
  = void $ runSTMStateT s (modify (set quantity B))
keyCallback s (GLFW.CharKey 'C') GLFW.Press
  = void $ runSTMStateT s (modify (over showCells not))
keyCallback _ _ _ = pure ()

colourPoint :: VisualisationState -> Point -> GL.Vector3 Float
colourPoint vs p | vs^.showCells = case p^.cell of
  Nothing -> colour
  Just c -> GL.Vector3
    (realToFrac (c^.dna.membraneDiffusion))
    (realToFrac (c^.dna.properties.key Photosynthesiser) / 4)
    0
                 | otherwise = colour
  where
    colour = (* realToFrac (p^.quantities.key (vs^.quantity) / 100)) <$> baseColour
    baseColour = case vs^.quantity of
      A -> GL.Vector3 0 0 1
      B -> GL.Vector3 0 1 0
      Energy -> GL.Vector3 1 0 0


worldToVertices :: VisualisationState -> World -> [Vertex]
worldToVertices vs w
  = [ (GL.Vector2 (fromIntegral x') (fromIntegral y'), colourPoint vs ((w^.grid)!v))
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

