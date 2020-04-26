module Main where

import System.Environment
import System.Exit

import Life
import TUI
import GUI

main :: IO ()
main = getArgs >>= \case
  ["gui"] -> mainOpenGL defaultShaderDir
  ["gui", shaderDir] -> mainOpenGL shaderDir
  ["tui"] -> loop (exampleWorld 10 10)
  _ -> putStrLn "Invalid arguments" >> exitFailure
  where
    defaultShaderDir = "res/shaders"
