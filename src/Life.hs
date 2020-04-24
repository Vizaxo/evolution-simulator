module Life where

import Control.Lens
import Data.Array
import Data.Map (Map)
import qualified Data.Map as M

data DNA = DNA
  deriving Show

data Cell = Cell
  { _dna :: DNA
  }
  deriving Show
makeLenses ''Cell

data Chemical = A
  deriving Show

data Point = Point
  { _chemicals :: Map Chemical Double
  , _cell :: Maybe Cell
  , _energy :: Double
  }
  deriving Show
makeLenses ''Point

data World = World
  { _width :: Int
  , _height :: Int
  , _grid :: Array (Int, Int) Point
  }
  deriving Show
makeLenses ''World

exampleWorld :: Int -> Int -> World
exampleWorld w h = World
  { _width = w
  , _height = h
  , _grid = array ((0,0),(w-1, h-1))
            [((x,y), initialPoint x y) | x <- [0..w-1], y <-[0..h-1]]
  }

printPoint :: Point -> Char
printPoint p = case p^.cell of
  Nothing -> '.'
  Just c -> '#'

printWorld :: World -> String
printWorld w = unlines [[printPoint ((w^.grid)!(x,y)) | x <- [0..w^.width-1]] | y <- [0..w^.height-1]]

initialPoint :: Int -> Int -> Point
initialPoint 5 6 = emptyPoint {_cell = Just (Cell DNA)}
initialPoint x y = emptyPoint

emptyPoint = Point
  { _chemicals = M.empty
  , _cell = Nothing
  , _energy = 0
  }
