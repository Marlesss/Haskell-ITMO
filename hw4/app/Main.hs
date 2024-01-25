module Main (main) where
--import HW4.T1
import HW4.T2
--import HW4.Types
main :: IO ()
main = let expr = "1 + 2.1234 * 3 * 4" in
           putStrLn $ show $ parseExpr expr