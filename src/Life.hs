module Life where

import Control.Concurrent
import Control.Lens hiding (indices)
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Maybe
import Data.TotalMap (TMap)
import qualified Data.TotalMap as M
import System.Random
import Linear

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

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

type Coord = V2 Int

data World = World
  { _width :: Int
  , _height :: Int
  , _grid :: Array Coord Point
  , _frame :: Int
  }
  deriving Show
makeLenses ''World

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

photosynthesisRate :: Double
photosynthesisRate = 1.5

diffusionRate :: Double
diffusionRate = 0.01

reproductionThreshold :: Double
reproductionThreshold = 3

-- The number of distinct calls to getRand in the program. Each call
-- should have a unique 'op' number from 0 to numRands-1. This ensures
-- random numbers are never re-used.
numRands :: Int
numRands = 1

--------------------------------------------------------------------------------
-- Simulation
--------------------------------------------------------------------------------

-- respiration: A -> B + energy
respiration :: Point -> Point
respiration p
  | p^.quantities.key A >= 1 && ((p^?cell._Just.dna.properties.key Photosynthesiser) == Just False)
  = over (quantities.key B) (+1) $
    over (quantities.key A) (subtract 1) $
    over (quantities.key Energy) (+2) $
    p
  | otherwise = p

-- photosynthesis: A + light (implicit) -> B
photosynthesis :: Point -> Point
photosynthesis p
  | p^.quantities.key B >= photosynthesisRate && ((p^?cell._Just.dna.properties.key Photosynthesiser) == Just True)
  = over (quantities.key A) (+photosynthesisRate) $
    over (quantities.key B) (subtract photosynthesisRate) $
    p
  | otherwise = p

diffusion :: World -> Coord -> Point -> Point
diffusion w v p = (foldr (.) id
                   ((\c -> over (quantities.key c) (diffuse c)) <$> [minBound..maxBound]))
                  p
  where
    dr00 :: Double
    dr00 = fromMaybe diffusionRate (p^?cell._Just.dna.membraneDiffusion)

    dr :: Coord -> Double
    dr v = min dr00 (fromMaybe diffusionRate
                     (w^?grid.ix v.cell._Just.dna.membraneDiffusion))

    diffuse :: Quantity -> Double -> Double
    diffuse c
      = let c00 = p^.quantities.key c
        in subtract $ sum
           [dr (v+dv) * (c00 - fromMaybe c00 (w^?grid.ix (v+dv).quantities.key c))
           | dv <- hexDirs]

willReproduce :: World -> Coord -> Point -> Bool
willReproduce w v p = isJust (p^.cell)
  && p^.quantities.key Energy > reproductionThreshold
  && getRand 0 w v (0.0, 1.0) > (0.9::Float)

reproduce :: World -> Coord -> Point -> Point
reproduce w v p
  | reproducing v
  = over (quantities.key Energy) (/3) p
  | reproducing (v - x)
  = set cell (fromJust (w^?grid.ix (v - x).cell))
    $ over (quantities.key Energy)
                      (+ (fromMaybe 0 (w^?grid.ix (v - x).quantities.key Energy) / 3)) p
  | otherwise = p
  where spaceToReproduce v = case w^?grid.ix v of
          Nothing -> False
          Just p -> isNothing (p^.cell)
        reproducing v = case w^?grid.ix v of
          Nothing -> False
          Just p' -> willReproduce w v p' && spaceToReproduce (v + x)

simPoint :: Point -> Point
simPoint = photosynthesis . respiration

simulate :: World -> World
simulate w
  = over frame (+1)
  $ over grid (mapWithIndex (\i -> reproduce w i . diffusion w i))
  $ over (grid.mapped) simPoint w

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

mapWithIndex :: Ix i => (i -> e -> e) -> Array i e -> Array i e
mapWithIndex f a = array (bounds a) [(i, f i (a!i)) | i <- indices a]

-- Lens into the element of a total map at a given key
key :: Ord k => k -> Lens' (TMap k a) a
key k = lens (M.! k) (flip (M.insert k))

showNDigits :: Int -> Int -> [Char]
showNDigits n x = reverse (take n (reverse (show x) ++ repeat '0'))

getRand :: Random a => Int -> World -> Coord -> (a, a) -> a
getRand op w v r = fst (randomR r (mkStdGen (mkSeed w (v^._x) (v^._y) op)))

mkSeed :: World -> Int -> Int -> Int -> Int
mkSeed w x y op = (((w^.frame)*(w^.height) + y)*(w^.width) + x)*numRands + op


--------------------------------------------------------------------------------
-- Hex grid
--------------------------------------------------------------------------------

-- The hex grid has is defined by its x (horizontal) and y (diagonal
-- up) coordinates, but a z vector pointing in the other diagonal to y
-- is also useful

x, y, z :: Coord
x = V2 1 0
y = V2 0 1
z = y - x

-- Vectors to the 6 adjacent hexes
hexDirs :: [Coord]
hexDirs = [x, -x, y, -y, z, -z]

--------------------------------------------------------------------------------
-- Initial setup
--------------------------------------------------------------------------------

exampleWorld :: Int -> Int -> World
exampleWorld w h = World
  { _width = w
  , _height = h
  , _grid = array (zero, (w-1)*^x + (h-1)*^y)
            [(v, initialPoint v)
            | x' <- (*^x) <$> [0..w-1]
            , y' <-(*^y) <$> [0..h-1]
            , let v = x' + y'
            ]
  , _frame = 0
  }

emptyDNA :: DNA
emptyDNA = DNA (M.empty False) diffusionRate

basicRespirator :: Cell
basicRespirator = Cell (set membraneDiffusion 0.005 emptyDNA)

basicPhotosynthesiser :: Cell
basicPhotosynthesiser = Cell (DNA (M.insert Photosynthesiser True (M.empty False)) 0.01)

initialPoint :: Coord -> Point
initialPoint (V2 4 7) = set cell (Just basicRespirator) emptyPoint
initialPoint (V2 3 8) = set cell (Just basicPhotosynthesiser) emptyPoint
initialPoint v = set (quantities.key A) 0 $ set (quantities.key B) 10 emptyPoint

emptyPoint = Point
  { _quantities = M.empty 0
  , _cell = Nothing
  }

--------------------------------------------------------------------------------
-- Visualisation
--------------------------------------------------------------------------------

printPoint :: Point -> [Char]
printPoint p = (if isJust (p^.cell) then '#' else ' '):(showNDigits 2 (floor x) ++ " ")
  where x = p^.quantities.key Energy

printWorld :: World -> String
printWorld w = unlines [concat $ replicate y' "  " ++
                         [printPoint ((w^.grid)!v)
                         | x' <- [0..w^.width-1], let v = x'*^x + y'*^y]
                       | y' <- [0..w^.height-1]]

loop :: MonadIO m => World -> m ()
loop w = void $ flip runStateT w $ forever $ do
  w <- get
  modify simulate
  liftIO $ putStrLn (printWorld w)
  liftIO $ threadDelay 100000
