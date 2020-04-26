module TUI where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Array
import Linear
import System.Console.ANSI

import Life

printPoint :: Point -> [Char]
printPoint p = case p^.cell of
  Nothing -> (" " ++ showNDigits 2 (floor x) ++ " ")
  Just c -> "#" ++ showNDigits 2 (floor (c^.dna.membraneDiffusion * 1000)) ++ " "
  where x = p^.quantities.key Energy

printWorld :: World -> String
printWorld w = unlines [concat $ replicate y' "  " ++
                         [printPoint ((w^.grid)!v)
                         | x' <- [0..w^.width-1], let v = x'*^x + y'*^y]
                       | y' <- [0..w^.height-1]]

loop :: MonadIO m => World -> m ()
loop w = liftIO $ go `finally` cleanup
  where
    go = do
      hideCursor
      void $ flip runStateT w $ forever $ do
        w <- get
        modify simulate
        liftIO $ do
          putStrLn (printWorld w)
          putStrLn ("Frame " ++ show (w^.frame))
          cursorUp printHeight
          threadDelay 10000
    cleanup = cursorDown printHeight >> showCursor
    printHeight = w^.height + 2
