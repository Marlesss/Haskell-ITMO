{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate
  , oneInfected

  , isVector
  , isHealthy
  , isDead
  ) where

import System.Random

import Data.Grid
import Control.Comonad (Comonad (..))
import Control.Lens
import Control.Lens.Extras (is)

data Config = Config
  { probability :: Double
  , incubationPeriod :: Int
  , illnessDuration :: Int
  , immunityDuration :: Int
  , mortality :: Double
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  | Dead
  deriving (Show, Eq)

makePrisms ''CellState

isVector :: CellState -> Bool
isVector cellState = is _Infected cellState || is _Ill cellState

isHealthy :: CellState -> Bool
isHealthy cellState = is _Healthy cellState || is _Immune cellState

isDead :: CellState -> Bool
isDead = is _Dead

data Cell = Cell
  { cellState :: CellState
  , cellRand :: StdGen
  } deriving Show

type Comonad19Grid = Grid Cell

-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).
simulate :: Config -> StdGen -> [Comonad19Grid]
simulate config stdGen = iterate (evolve config) $ oneInfected stdGen

oneInfected :: StdGen -> Comonad19Grid
oneInfected stdGen = gGenerator (splitStdGen (fst . fst))
                                (splitStdGen (fst . snd))
                                (splitStdGen (snd . fst))
                                (splitStdGen (snd . snd))
                                (Cell (Infected 0) stdGen) where
  split4 sg = let (sg1, sg2) = split sg in (split sg1, split sg2)
  splitStdGen choice cell = let splittedStdGen = choice $ split4 (cellRand cell) in
    Cell Healthy splittedStdGen

neighborsDirections :: [Comonad19Grid -> Comonad19Grid]
neighborsDirections = horizontals ++ verticals
  where horizontals = [gLeft, gRight]
        verticals   = [gUp, gDown]

neighbors :: Comonad19Grid -> [Cell]
neighbors grid = map (\direction -> extract $ direction grid) neighborsDirections

neighborsSatisfy :: Comonad19Grid -> (Cell -> Bool) -> Int
neighborsSatisfy grid p = length $ filter p $ neighbors grid

contractingProbability :: Config -> Comonad19Grid -> Double
contractingProbability conf g = 1 - ((1 - probability conf) ^^ neighborsSatisfy g (isVector . cellState))

mortalityDayProbability :: Config -> Double
mortalityDayProbability conf = 1 - (1 - mortality conf) ** ((1 :: Double) / fromIntegral (illnessDuration conf))

rule :: Config -> Comonad19Grid -> Cell
rule conf@(Config { .. }) g = let (Cell {..}) = extract g
                                  (stdGen, nextStdGen) = split cellRand in flip Cell nextStdGen $ case cellState of
  Healthy      | checkProbability stdGen (contractingProbability conf g) -> Infected 0
               | otherwise                                               -> Healthy

  (Infected i) | i >= incubationPeriod - 1                               -> Ill 0
               | otherwise                                               -> Infected (i + 1)

  (Ill i)      | checkProbability stdGen (mortalityDayProbability conf)  -> Dead
               | i >= illnessDuration - 1                                -> Immune 0
               | otherwise                                               -> Ill (i + 1)

  (Immune i)   | i >= immunityDuration - 1                               -> Healthy
               | otherwise                                               -> Immune (i + 1)

  Dead                                                                   -> Dead

evolve :: Config -> Comonad19Grid -> Comonad19Grid
evolve config = extend (rule config)

checkProbability :: StdGen -> Double -> Bool
checkProbability stdGen probability = let (rand, _) = random stdGen in rand <= probability
