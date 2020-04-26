module GUI where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import System.FSNotify

import STMState
import OpenGL

mainOpenGL :: FilePath -> IO ()
mainOpenGL shaderDir = do
  makeWindow
  initOGL
  rs <- initRenderState shaderDir

  s <- liftIO (newTVarIO rs)
  recompileOnChange s shaderDir
  postInit s

  t0 <- liftIO (getCurrentTime)
  void $ runSTMStateT s $ do
    forever $ do
      (iTime, t) <- elapsedTime t0
      renderFrame iTime t

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
