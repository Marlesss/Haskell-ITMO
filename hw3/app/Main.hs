module Main (main) where

import HW3.T4

main :: IO ()
main = let expr = signum $ abs (5 - 3 * 2 / 4) :: Expr in
    putStrLn $ show $ runS (eval expr) []
