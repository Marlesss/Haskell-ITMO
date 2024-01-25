module Main (main) where
import HW1.T1
import HW1.T2
import HW1.T3

main :: IO ()
main = let tree = tFromList [1, 5, 2, 6, 2, 5]
       in putStrLn $ show tree
