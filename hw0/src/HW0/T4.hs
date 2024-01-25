module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function(fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \rec list -> case list of
  []     -> []
  a:rest -> f a : rec rest

fib :: Natural -> Natural
fib = fix (\rec a b n -> if n == 0 then a else rec b (a + b) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\rec x -> if x <= 1 then 1 else x * rec (x - 1))
