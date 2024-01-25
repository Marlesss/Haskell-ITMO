{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name '[] = 'False
  Contains name (name ': tail) = 'True
  Contains name (_ ': tail) = Contains name tail

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete name '[] = '[]
  Delete name (name ': tail) = tail
  Delete name (otherName ': tail) = otherName ': Delete name tail

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add v set = v ': set
