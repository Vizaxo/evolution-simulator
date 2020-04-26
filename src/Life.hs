module Life where

import Control.Lens hiding (indices)
import Data.Array
import Data.Bits
import Data.Maybe
import Data.TotalMap (TMap)
import qualified Data.TotalMap as M
import Data.Word
import Linear

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Gene = Photosynthesiser
  deriving (Eq, Ord, Show)

data DNA = DNA
  { _properties :: TMap Gene Double
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

respirationRate :: Double
respirationRate = 1.0

diffusionRate :: Double
diffusionRate = 0.001

reproductionThreshold :: Double
reproductionThreshold = 50

-- The number of distinct calls to getRand in the program. Each call
-- should have a unique 'op' number from 0 to numRands-1. This ensures
-- random numbers are never re-used.
numRands :: Int
numRands = 4

--------------------------------------------------------------------------------
-- Simulation
--------------------------------------------------------------------------------

-- respiration: A -> B + energy
respiration :: Point -> Point
respiration p
  = case p^.cell of
      Nothing -> p
      Just c -> if p^.quantities.key A >= respirationRate
                then respire p
                else kill p
  where respire = over (quantities.key B) (+respirationRate) .
                  over (quantities.key A) (subtract respirationRate) .
                  over (quantities.key Energy) (+0.3 * respirationRate)

kill :: Point -> Point
kill = set cell Nothing

-- photosynthesis: B + light (implicit) -> A
photosynthesis :: Point -> Point
photosynthesis p
  = case (p^?cell._Just.dna.properties.key Photosynthesiser) of
      Nothing -> p
      Just amount -> if p^.quantities.key B >= photosynthesisRate*amount then
                       over (quantities.key A) (+photosynthesisRate*amount) $
                       over (quantities.key B) (subtract (photosynthesisRate*amount)) $
                       p
                     else p

diffusion :: World -> Coord -> Point -> Point
diffusion w v p = (foldr (.) id
                   ((\c -> over (quantities.key c) (diffuse c)) <$> [minBound..maxBound]))
                  p
  where
    dr00 :: Double
    dr00 = max 0 (min (1/6) (fromMaybe diffusionRate (p^?cell._Just.dna.membraneDiffusion)))

    dr :: Coord -> Double
    dr v = max 0 (min (1/6) (min dr00 (fromMaybe diffusionRate
                     (w^?grid.ix v.cell._Just.dna.membraneDiffusion))))

    diffuse :: Quantity -> Double -> Double
    diffuse c
      = let c00 = p^.quantities.key c
        in subtract $ sum
           [dr (v+dv) * (c00 - fromMaybe c00 (w^?grid.ix (v+dv).quantities.key c))
           | dv <- hexDirs]

willReproduce :: World -> Coord -> Maybe Coord
willReproduce w v = case w^?grid.ix v of
  Nothing -> Nothing
  Just p -> if isJust (p^.cell)
    && p^.quantities.key Energy > reproductionThreshold
    && getRand 0 w v `mod` 1000 >= 900
    then Just pos else Nothing
  where
    pos = v + (hexDirs !! (getRand 1 w v `mod` 6))

reproduce :: World -> Coord -> Point -> Point
reproduce w v p
  | reproducing v
  = over (quantities.key Energy) (/3) p
  | spaceToReproduce v
  = case tryingToReproduceInto v of
      [(_, v')] -> set cell (mutate <$> (fromJust (w^?grid.ix v'.cell)))
                   $ over (quantities.key Energy)
                   (const 0) p --(+ (fromMaybe 0 (w^?grid.ix v'.quantities.key Energy) / 10000)) p
      _ -> p
  | otherwise = p
  where spaceToReproduce v = case w^?grid.ix v of
          Nothing -> False
          Just p -> isNothing (p^.cell) && length (tryingToReproduceInto v) <= 1
        tryingToReproduceInto v = filter ((== Just v) . fst)
          [(willReproduce w (v+dv), v+dv) | dv <- hexDirs]
        reproducing v = case willReproduce w v of
                          Nothing -> False
                          Just v' -> spaceToReproduce v'
        mutate = over (dna.properties.key Photosynthesiser) (+ jitter 0)
          . over (dna.membraneDiffusion) (+ jitter 1)
        jitter n = 1 / fromIntegral ((getRand (2+n) w v `mod` 10) + 1)

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

getRand :: Num a => Int -> World -> Coord -> a
getRand op w v = fromIntegral (hash (mkSeed w (v^._x) (v^._y) op))

mkSeed :: World -> Int -> Int -> Int -> Word32
mkSeed w x y op = fromIntegral ((((w^.frame)*(w^.height) + y)*(w^.width) + x)*numRands + op)

hash :: Word32 -> Word32
hash x = let x1 = ((x `shiftR` 16) `xor` x) * 0x45d9f3b
             x2 = ((x1 `shiftR` 16) `xor` x1) * 0x45d9f3b
             x3 = ((x2 `shiftR` 16) `xor` x2)
         in x3

unhash :: Word32 -> Word32
unhash x = let x1 = ((x `shiftR` 16) `xor` x) * 0x119de1f3
               x2 = ((x1 `shiftR` 16) `xor` x1) * 0x119de1f3
               x3 = ((x2 `shiftR` 16) `xor` x2)
           in x3

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
emptyDNA = DNA (M.empty 0) diffusionRate

basicRespirator :: Cell
basicRespirator = Cell (set membraneDiffusion 0.005 emptyDNA)

basicPhotosynthesiser :: Cell
basicPhotosynthesiser = Cell (DNA (M.insert Photosynthesiser 1 (M.empty 0)) 0.01)

initialPoint :: Coord -> Point
initialPoint (V2 4 7) = set (quantities.key A) 100 $ set cell (Just basicRespirator) emptyPoint
initialPoint (V2 3 8) = set (quantities.key A) 10 $ set cell (Just basicPhotosynthesiser) emptyPoint
initialPoint v = set (quantities.key A) 2 $ set (quantities.key B) 2 emptyPoint

emptyPoint = Point
  { _quantities = M.empty 0
  , _cell = Nothing
  }
