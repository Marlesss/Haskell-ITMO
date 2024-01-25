module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import HW0.T1 (distrib)
import Data.Char (isSpace)

a :: (Either String b, Either String c)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

--as fully applied function
--distrib (Left ("AB" ++ "CD" ++ "EF")) ==> (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))

---- Defined in ‘ghc-prim-0.6.1:GHC.Tuple’
--type (,) :: * -> * -> *
--data (,) a b = (,) a b

--the expression is fully-applied (,) constructor without strict fields
--so it is in WHNF (even without evaluating Left ("AB" ++ "CD" ++ "EF") not a single step

a_whnf :: (Either String b, Either String c)
a_whnf = let val = "AB" ++ "CD" ++ "EF" in (Left val, Left val)
a_whnf = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))



b :: [Bool]
b = map isSpace "Hello, World"

---- Defined in ‘GHC.Base’
--map :: (a -> b) -> [a] -> [b]
--map _ []     = []
--map f (x:xs) = f x : map f xs

--it is a fully-applied map function so
--map isSpace 'H':"ello, World" ==> isSpace 'H' : map isSpace "ello, World"

---- Defined in ‘ghc-prim-0.6.1:GHC.Types’
--type [] :: * -> *
--data [] a = ... | a : [a]
--infixr 5 :

--the expression is a fully-applied (:) constructor without strict fields
--so it is in WHNF (even without evaluating arguments not a single step)
b_whnf :: [Bool]
b_whnf = isSpace 'H' : map isSpace "ello, World"

c :: [Char]
c = if (1 :: Integer) > 0 || error "X" then "Y" else "Z"

--didn't find any sources of 'if' statement :(
--whatever 'if' is, it is fully-applied function, so it's evaluates
--we know that 'if' is lazy and behaves like
--if True a _ = a
--if False _ b = b

--     Defined in ‘ghc-prim-0.6.1:GHC.Classes’
--    (||) :: Bool -> Bool -> Bool
--    True  || _ =  True
--    False || x =  x

--    so || is lazy in the second argument

--        1 > 0 ==> True
--    True || _ = True
--if True a _ = a
--so expression's WHNF is A's WHNF
--    "Y" === 'Y' : []
--'Y' : [] is a fully-applied (:) constructor without strict fields
--so it is in WHNF (without evaluating 'Y' and [])
c_whnf :: [Char]
c_whnf = 'Y' : []