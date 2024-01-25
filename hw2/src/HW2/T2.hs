module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = []:|[]
splitOn t a =  foldr (addList . (\x ->  [x | x /= t])) ([] :| []) a where
    addList el (x:|xs)
        | null el = [] :| x : xs
        | otherwise = (el ++ x):|xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith t = foldr1 (\l r -> l ++ [t] ++ r)