module Life where

import Control.Concurrent
import Control.Lens hiding (indices)
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Maybe
import Data.TotalMap (TMap)
import qualified Data.TotalMap as M

data Gene = Photosynthesiser
  deriving (Eq, Ord, Show)

data DNA = DNA
  { _properties :: TMap Gene Bool
  , _membraneDiffusion :: Double
  }
  deriving Show
makeLenses ''DNA

data Cell = Cell
  { _dna :: DNA
  }
  deriving Show
makeLenses ''Cell

data Quantity = A | B | Energy
  deriving (Eq, Ord, Show, Enum, Bounded)

data Point = Point
  { _quantities :: TMap Quantity Double
  , _cell :: Maybe Cell
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
printPoint p = toEnum $ fromEnum '0' + floor ((p^.quantities.key A))

printWorld :: World -> String
printWorld w = unlines [[printPoint ((w^.grid)!(x,y)) | x <- [0..w^.width-1]] | y <- [0..w^.height-1]]

emptyDNA :: DNA
emptyDNA = DNA (M.empty False) diffusionRate

basicRespirator :: Cell
basicRespirator = Cell (set membraneDiffusion 0.001 emptyDNA)

basicPhotosynthesiser :: Cell
basicPhotosynthesiser = Cell (DNA (M.insert Photosynthesiser True (M.empty False)) 0.002)

initialPoint :: Int -> Int -> Point
initialPoint 4 7 = set cell (Just basicRespirator) emptyPoint
initialPoint 3 8 = set cell (Just basicPhotosynthesiser) emptyPoint
initialPoint x y = set (quantities.key B) 5 emptyPoint

emptyPoint = Point
  { _quantities = M.empty 0
  , _cell = Nothing
  }

-- Lens into the element of a total map at a given key
key :: Ord k => k -> Lens' (TMap k a) a
key k = lens (M.! k) (flip (M.insert k))

-- respiration: A -> B + energy
respiration :: Point -> Point
respiration p
  | p^.quantities.key A >= 1 && ((p^?cell._Just.dna.properties.key Photosynthesiser) == Just False)
  = over (quantities.key B) (+1) $
    over (quantities.key A) (subtract 1) $
    over (quantities.key Energy) (+2) $
    p
  | otherwise = p

photosynthesisRate :: Double
photosynthesisRate = 1.5

-- photosynthesis: A + light (implicit) -> B
photosynthesis :: Point -> Point
photosynthesis p
  | p^.quantities.key B >= photosynthesisRate && ((p^?cell._Just.dna.properties.key Photosynthesiser) == Just True)
  = over (quantities.key A) (+photosynthesisRate) $
    over (quantities.key B) (subtract photosynthesisRate) $
    p
  | otherwise = p

diffusionRate :: Double
diffusionRate = 0.01

diffusion :: World -> (Int, Int) -> Point -> Point
diffusion w (x, y) p = (foldr (.) id $ fmap (\c -> over (quantities.key c) (diff c)) [minBound..maxBound]) p
  where
    dr00 :: Double
    dr00 = case p^.cell of
      Nothing -> diffusionRate
      Just c -> c^.dna.membraneDiffusion

    diff :: Quantity -> Double -> Double
    diff c = subtract
      (sum [dr (x+dx) (y+dy) * factor dx dy * (c00 - fromMaybe c00 (w^?grid.ix (x+dx, y+dy).quantities.key c))
        | dx <- [-1..1], dy <- [-1..1]])
      where

        c00 = p^.quantities.key c
        dr x y = min dr00 (fromMaybe diffusionRate (w^?grid.ix (x, y).cell._Just.dna.membraneDiffusion))
    factor x y | abs x == abs y = 1 / sqrt 2
               | otherwise = 1

simPoint :: Point -> Point
simPoint = photosynthesis . respiration

mapWithIndex :: Ix i => (i -> e -> e) -> Array i e -> Array i e
mapWithIndex f a = array (bounds a) [(i, f i (a!i)) | i <- indices a]

simulate :: World -> World
simulate w = over grid (mapWithIndex (diffusion w)) $ over (grid.mapped) simPoint w

loop :: MonadIO m => World -> m ()
loop w = void $ flip runStateT w $ forever $ do
  w <- get
  modify simulate
  liftIO $ putStrLn (printWorld w)
  liftIO $ threadDelay 100000
