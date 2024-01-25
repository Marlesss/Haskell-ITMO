-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , lLeft
  , lRight
  , iterateTail
  , lGenerator
  , zipperToList
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x

  duplicate = lGenerator lLeft lRight

lLeft, lRight :: ListZipper a -> ListZipper a

lLeft  (LZ (l : ls) c rs) = LZ ls l (c : rs)
lLeft  lz = lz

lRight (LZ ls c (r : rs)) = LZ (c : ls) r rs
lRight lz = lz

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

lGenerator :: (a -> a)
           -> (a -> a)
           -> a
           -> ListZipper a
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)

zipperToList :: ListZipper a -> Int -> [a]
zipperToList (LZ ls x rs) n = let half = (n - 1) `div` 2 in
  reverse (take half ls) ++ [x] ++ take (half + (n - 1) `mod` 2) rs
