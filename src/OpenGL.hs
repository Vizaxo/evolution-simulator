module OpenGL where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Time.Clock
import Foreign.Storable
import Foreign.Storable.Tuple()
import Graphics.GLUtil
import Graphics.Rendering.OpenGL (($=))
import System.Exit
import System.IO
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import STMState

data RenderState = RenderState
  { _shaderProg :: ShaderProgram
  , _vao :: GL.VertexArrayObject
  , _windowSize :: GL.Size
  , _lastRenderTime :: UTCTime
  , _dirty :: Bool
  , _shaderDir :: FilePath
  }
makeLenses ''RenderState

type Vertex = (GL.Vector2 Float, GL.Vector3 Float)

vertices :: [Vertex]
vertices = [ (GL.Vector2 0 0, GL.Vector3 1 0 0)
           , (GL.Vector2 1 0, GL.Vector3 0 1 0)
           , (GL.Vector2 0 1, GL.Vector3 0 0 1)
           ]

makeWindow :: MonadIO m => m ()
makeWindow = liftIO $ do
  GLFW.initialize
  GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
  GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
  GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
  GLFW.openWindow (GL.Size 400 600) [] GLFW.Window
  GLFW.windowCloseCallback $= exitSuccess

  -- Disable vsync
  GLFW.swapInterval $= 0

initOGL :: MonadIO m => m ()
initOGL = do
  GL.debugOutput $= GL.Enabled
  GL.polygonMode $= (GL.Fill, GL.Fill)
  GL.cullFace $= Nothing
  GL.depthFunc $= Just GL.Less
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 0.0

initRenderState :: MonadIO m => FilePath -> m RenderState
initRenderState shaderDir = do
  shaderProg <- compileShaders shaderDir >>= \case
    Just sp -> pure sp
    Nothing -> error "Shader compilation failed"

  windowSize <- GL.get GLFW.windowSize

  vao <- liftIO $ makeVAO $ do
      let stride = fromIntegral (sizeOf (undefined :: Vertex))
          pos = GL.VertexArrayDescriptor 2 GL.Float stride offset0
          col = GL.VertexArrayDescriptor 3 GL.Float stride
            (offsetPtr (sizeOf (undefined :: GL.Vector2 Float)))
          posAttribute  = getAttrib shaderProg "pos"
          colAttribute  = getAttrib shaderProg "colIn"
      vbo <- makeBuffer GL.ArrayBuffer vertices
      GL.vertexAttribArray posAttribute $= GL.Enabled
      GL.vertexAttribPointer posAttribute $= (GL.ToFloat, pos)
      GL.vertexAttribArray colAttribute $= GL.Enabled
      GL.vertexAttribPointer colAttribute $= (GL.ToFloat, col)
  GL.bindVertexArrayObject $= Just vao

  currentTime <- liftIO getCurrentTime
  pure (RenderState shaderProg vao windowSize currentTime False shaderDir)

postInit :: MonadIO m => TVar RenderState -> m ()
postInit s = do
  GLFW.windowSizeCallback $= updateWindowSize s
  GLFW.keyCallback $= keyCallback s

keyCallback :: TVar RenderState -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
keyCallback _ (GLFW.SpecialKey GLFW.ESC) GLFW.Press
  = exitSuccess
keyCallback _ _ _ = pure ()

updateWindowSize :: MonadIO m => TVar RenderState -> GL.Size -> m ()
updateWindowSize s size = void $ runSTMStateT s $ do
  modify (set windowSize size)
  liftIO (GL.viewport $= (GL.Position 0 0, size))

makeShaderProgram :: FilePath -> IO ShaderProgram
makeShaderProgram shaderDir = loadShaderProgram
  [ (GL.VertexShader, shaderDir ++ "/vertex.glsl")
  , (GL.GeometryShader, shaderDir ++ "/geometry.glsl")
  , (GL.FragmentShader, shaderDir ++ "/fragment.glsl")
  ]

recompileIfDirty :: (MonadState RenderState m, MonadIO m) => m ()
recompileIfDirty = do
  rs <- get
  when (rs^.dirty) $ do
    compileShaders (rs^.shaderDir) >>= \case
      Nothing -> modify (set dirty False)
      Just sp -> modify (set dirty False . set shaderProg sp)

compileShaders :: MonadIO m => FilePath -> m (Maybe ShaderProgram)
compileShaders shaderDir = do
  liftIO (try (makeShaderProgram shaderDir)) >>= \case
    Right sp -> do
      GL.currentProgram $= Just (program sp)
      liftIO (putStrLn " Recompiled\n")
      pure (Just sp)
    Left e -> do
      liftIO (print (e :: IOException))
      liftIO (putStrLn " Shader compilation failed\n")
      pure Nothing

uniformExists :: GL.UniformLocation -> Bool
uniformExists (GL.UniformLocation (-1)) = False
uniformExists _ = True

-- Set a uniform without giving a warning or error if it is not active
safeSetUniform :: (GL.Uniform a, MonadGet RenderState m, MonadIO m)
  => String -> a -> m ()
safeSetUniform name v = do
  rs <- get
  uLocation <- GL.get (GL.uniformLocation (rs^.shaderProg&program) name)
  when (uniformExists uLocation) $ GL.uniform uLocation $= v

bindTexture :: (MonadGet RenderState m, MonadIO m)
  => GL.TextureObject -> String -> GL.TextureUnit -> m ()
bindTexture texture uniformName textureUnit = do
  GL.activeTexture $= textureUnit
  GL.textureBinding GL.Texture2D $= Just texture
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  texture2DWrap $= (GL.Repeated, GL.Repeat)
  safeSetUniform uniformName textureUnit

renderToScreen :: (MonadGet RenderState m, MonadIO m) => m ()
renderToScreen = do
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  liftIO $ GL.drawArrays GL.Points 0 (fromIntegral (length vertices))
  liftIO $ GLFW.swapBuffers

renderFrame :: (MonadState RenderState m, MonadIO m) => Float -> UTCTime -> m ()
renderFrame iTime t = do
  GL.get GL.errors >>= \case
    [] -> pure ()
    es -> liftIO $ print es

  recompileIfDirty

  rs <- get

  let tPrev = rs^.lastRenderTime
      dt = realToFrac (diffUTCTime t tPrev) :: Float
      fps = 1.0 / dt
  modify (set lastRenderTime t)

  let (GL.Size width height) = rs^.windowSize
  safeSetUniform "scaleX" (0.2 :: GL.GLfloat)
  safeSetUniform "scaleY" ((0.2 / fromIntegral height * fromIntegral width) :: GL.GLfloat)
  renderToScreen

  liftIO $ putStr $ "FPS: " ++ show fps ++
    "fps. Resolution: " ++ show width ++ "*" ++ show height ++ "    \r"
  liftIO $ hFlush stdout
