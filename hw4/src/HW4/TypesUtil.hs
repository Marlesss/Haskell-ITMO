module HW4.TypesUtil
  ( mapAnnotated
  , mapExcept
  , joinExcept
  , primApply
  ) where

import HW4.Types

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated func (a :# e) = func a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept func (Success a) = Success $ func a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success a) = a

primApply :: Fractional a => Prim a -> a
primApply (Add a b) = a + b
primApply (Sub a b) = a - b
primApply (Mul a b) = a * b
primApply (Div a b) = a / b
primApply (Abs a) = abs a
primApply (Sgn a) = signum a
