{-# LANGUAGE RecordWildCards #-}

module Options.Parser
  ( AppConfig(..)
  , OptionsError(..)
  , parse
  , validateAppConfig
  ) where

import HW6.T3(Config(..))
import Options.Applicative
import Control.Monad.Trans.Except

data OptionsError =
  MustBePositive String
  | MustBeProbability String
  deriving Show

validateAppConfig :: AppConfig -> Except OptionsError AppConfig
validateAppConfig c@(AppConfig {..})
  | gridSize <= 0                                     = throwE $ MustBePositive "GRID_SIZE"
  | iterations <= 0                                   = throwE $ MustBePositive "ITERATIONS"
  | not $ isProbability $ probability comonad19Config = throwE $ MustBeProbability "P"
  | incubationPeriod comonad19Config <= 0             = throwE $ MustBePositive "INCUB"
  | illnessDuration comonad19Config <= 0              = throwE $ MustBePositive "ILL"
  | immunityDuration comonad19Config <= 0             = throwE $ MustBePositive "IMMUN"
  | not $ isProbability $ mortality comonad19Config   = throwE $ MustBeProbability "MORT"
  | otherwise                                         = pure c

isProbability :: Double -> Bool
isProbability x = 0 <= x && x <= 1

data AppConfig = AppConfig
  { gridSize        :: Int
  , iterations      :: Int
  , comonad19Config :: Config
  }

parse :: IO AppConfig
parse = execParser pInfoConfig

pInfoConfig :: ParserInfo AppConfig
pInfoConfig = info (pAppConfig <**> helper)
  ( fullDesc
    <> header "comonad19 - Simulation of the epidemic spread" )

pOption :: (Read a, Show a) => String -> String -> a -> String -> Parser a
pOption optName varName defVal helpMessage = option auto
  ( long optName
 <> metavar varName
 <> value defVal
 <> showDefault
 <> help helpMessage )

pAppConfig :: Parser AppConfig
pAppConfig = AppConfig
  <$> pGridSize
  <*> pIterations
  <*> pConfig

pGridSize :: Parser Int
pGridSize = pOption "grid-size" "GRID_SIZE" (gridSize defaultAppConfig)
  "Output grid size. Note that simulation grid is infinite."

pIterations :: Parser Int
pIterations = pOption "iterations" "ITERATIONS" (iterations defaultAppConfig)
  "The number of simulation iterations"

pConfig :: Parser Config
pConfig = Config
  <$> pProbability
  <*> pIncubation
  <*> pIllness
  <*> pImmunity
  <*> pMortality

pProbability :: Parser Double
pProbability = pOption "prob" "P" (probability $ comonad19Config defaultAppConfig)
  "Infection probability"

pIncubation :: Parser Int
pIncubation = pOption "incub" "INCUB" (incubationPeriod $ comonad19Config defaultAppConfig)
  "Incubation period duration"

pIllness :: Parser Int
pIllness = pOption "ill" "ILL" (illnessDuration $ comonad19Config defaultAppConfig)
  "Illness duration"

pImmunity :: Parser Int
pImmunity = pOption "immun" "IMMUN" (immunityDuration $ comonad19Config defaultAppConfig)
  "Immunity duration"

pMortality :: Parser Double
pMortality = pOption "mort" "MORT" (mortality $ comonad19Config defaultAppConfig)
  "Mortality probability (at one time of illness)"

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
  { gridSize   = 6
  , iterations = 10
  , comonad19Config = Config
    { probability      = 0.35
    , incubationPeriod = 5
    , illnessDuration  = 7
    , immunityDuration = 14
    , mortality        = 0.05
    }
  }