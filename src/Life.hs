module Life where

import Control.Concurrent
import Control.Lens hiding (indices)
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Maybe
import Data.TotalMap (TMap)
import qualified Data.TotalMap as M

data DNA = DNA
  deriving Show

data Cell = Cell
  { _dna :: DNA
  }
  deriving Show
makeLenses ''Cell

data Chemical = A | B
  deriving (Eq, Ord, Show, Enum, Bounded)

data Point = Point
  { _chemicals :: TMap Chemical Double
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
printPoint p = toEnum $ fromEnum '0' + floor ((p^.chemicals.key B))

printWorld :: World -> String
printWorld w = unlines [[printPoint ((w^.grid)!(x,y)) | x <- [0..w^.width-1]] | y <- [0..w^.height-1]]

initialPoint :: Int -> Int -> Point
initialPoint 5 6 = emptyPoint {_cell = Just (Cell DNA)}
initialPoint x y = set (chemicals.key A) 9 emptyPoint

emptyPoint = Point
  { _chemicals = M.empty 0
  , _cell = Nothing
  , _energy = 0
  }

-- Lens into the element of a total map at a given key
key :: Ord k => k -> Lens' (TMap k a) a
key k = lens (M.! k) (flip (M.insert k))

-- respiration: A -> B + energy
respiration :: Point -> Point
respiration p
  | p^.chemicals.key A >= 1 && isJust (p^.cell) = over (chemicals.key B) (+1)
                                                  $ over (chemicals.key A) (subtract 1) $ p
  | otherwise = p

diffusionRate :: Double
diffusionRate = 0.01

diffusion :: World -> (Int, Int) -> Point -> Point
diffusion w (x, y) p = (foldr (.) id $ fmap (\c -> over (chemicals.key c) (diff c)) [minBound..maxBound]) p
  where
    diff :: Chemical -> Double -> Double
    diff c = subtract
      (diffusionRate * sum
        [factor dx dy * (c00 - fromMaybe c00 (w^?grid.ix (x+dx, y+dy).chemicals.key c))
        | dx <- [-1..1], dy <- [-1..1]])
      where
        c00 = p^.chemicals.key c
    factor x y | abs x == abs y = 1 / sqrt 2
               | otherwise = 1

simPoint :: Point -> Point
simPoint = respiration

mapWithIndex :: Ix i => (i -> e -> e) -> Array i e -> Array i e
mapWithIndex f a = array (bounds a) [(i, f i (a!i)) | i <- indices a]

simulate :: World -> World
simulate w = over grid (mapWithIndex (diffusion w)) $ over (grid.mapped) simPoint w

loop :: MonadIO m => World -> m ()
loop w = void $ flip runStateT w $ forever $ do
  w <- get
  modify simulate
  liftIO $ putStrLn (printWorld w)
  liftIO $ threadDelay 200000
