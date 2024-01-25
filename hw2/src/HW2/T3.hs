module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart [] = (mempty, mempty)
epart x = foldMap wrap x where
    wrap t = case t of
            Left a -> (a, mempty)
            Right b -> (mempty, b)
