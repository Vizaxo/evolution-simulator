module Main where

import Life
import TUI

main :: IO ()
main = loop (exampleWorld 10 10)
