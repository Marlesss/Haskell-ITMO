{-# LANGUAGE DeriveFunctor #-}

-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gUp
  , gDown
  , gLeft
  , gRight
  , gGenerator
  , gridToList
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) } deriving Functor

instance Comonad Grid where
  extract = extract . extract . unGrid

  duplicate = gGenerator gUp gDown gLeft gRight

gUp, gDown, gLeft, gRight :: Grid a -> Grid a
gUp    (Grid g) = Grid (lLeft  g)
gDown  (Grid g) = Grid (lRight g)
gLeft  (Grid g) = Grid (fmap lLeft  g)
gRight (Grid g) = Grid (fmap lRight g)

gGenerator :: (a -> a) -- ^ The up generator
           -> (a -> a) -- ^ The down generator
           -> (a -> a) -- ^ The left generator
           -> (a -> a) -- ^ The right generator
           -> a        -- ^ The focus
           -> Grid a   -- ^ The resulting grid
gGenerator upF downF leftF rightF = Grid . fmap (lGenerator leftF rightF) . lGenerator upF downF

gridToList :: Int -> Grid a -> [[a]]
gridToList n (Grid {unGrid = grid}) = map (`zipperToList` n) (zipperToList grid n)
