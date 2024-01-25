module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

data Option a = None | Some a
  deriving Show

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None = None
mapOption func (Some a) = Some $ func a

data Pair a = P a a
  deriving Show

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair func (P a b) = P (func a) (func b)

data Quad a = Q a a a a
  deriving Show

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad func (Q a b c d) = Q (func a) (func b) (func c) (func d)

data Annotated e a = a :# e
  deriving Show

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated func (a :# e) = func a :# e

data Except e a = Error e | Success a
  deriving Show

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept func (Success a) = Success $ func a

data Prioritised a = Low a | Medium a | High a
  deriving Show

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised func (Low a) = Low $ func a
mapPrioritised func (Medium a) = Medium $ func a
mapPrioritised func (High a) = High $ func a

data Stream a = a :> Stream a
  deriving Show

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream func (a :> t) = func a :> mapStream func t

data List a = Nil | a :. List a
  deriving Show

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList func (a :. list) = func a :. mapList func list

data Fun i a = F (i -> a)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g e = f $ g e

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun func (F a) = F $ compose func a

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree func (Branch l m r) = Branch (mapTree func l) (func m) (mapTree func r)
