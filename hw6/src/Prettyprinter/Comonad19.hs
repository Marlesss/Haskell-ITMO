module Prettyprinter.Comonad19
  ( prettyComonad19Grids
  , prettyOptionsError
  , putDocLn
  ) where

import HW6.T3
import Data.Grid(gridToList)
import Options.Parser(OptionsError(..))

import Prettyprinter hiding (prettyList)
import Prettyprinter.Render.Terminal

prettyOptionsError :: OptionsError -> Doc AnsiStyle
prettyOptionsError e = annotate (color Red) $ pretty $ case e of
  (MustBePositive var)    -> var ++ " must be positive integer"
  (MustBeProbability var) -> var ++ " must be probability (0 <= " ++ var ++ " <= 1)"

prettyComonad19Grids :: Int -> [Comonad19Grid] -> Doc AnsiStyle
prettyComonad19Grids n grids = concatWith (\a b -> a <> hardline <> hardline <> hardline <> b)
                               $ map (prettyComonad19Grid n) grids

prettyComonad19Grid :: Int -> Comonad19Grid -> Doc AnsiStyle
prettyComonad19Grid n grid = reAnnotate cellColoring $ prettyComonad19GridAnnotated n grid

cellColoring :: CellState -> AnsiStyle
cellColoring Healthy      = bgColorDull Green
cellColoring (Infected _) = bgColorDull Red
cellColoring (Ill _)      = bgColor     Red
cellColoring (Immune _)   = bgColorDull White
cellColoring Dead         = bgColor     Black

prettyComonad19GridAnnotated :: Int -> Comonad19Grid -> Doc CellState
prettyComonad19GridAnnotated n grid = ( concatWith (\a b -> a <> hardline <> b)
                                      $ map (mconcat . map prettyCell)
                                      $ gridToList n grid )
                                      <> hardline <> comonad19Stat n grid

comonad19Stat :: Int -> Comonad19Grid -> Doc CellState
comonad19Stat n g = let total = n * n
                        cells = map cellState $ concat $ gridToList n g in
  concatWith (\a b -> a <> hardline <> b)
    $ map (\(predicate, name, state) -> do
            let satisfyCount = length $ filter predicate cells
            annotate state $ pretty $ name ++ " " ++ show satisfyCount ++ "/" ++ show total)
          [(isHealthy, "healthy", Healthy), (isVector, "vector", Ill 0), (isDead, "dead", Dead)]

prettyCell :: Cell -> Doc CellState
prettyCell (Cell {cellState = state, cellRand = _}) = annotate state $ pretty $ case state of
  Healthy      -> "_"
  (Infected _) -> "i"
  (Ill _)      -> "#"
  (Immune _)   -> "@"
  Dead         -> "X"

putDocLn :: Doc AnsiStyle -> IO ()
putDocLn d = putDoc d >> putStrLn ""