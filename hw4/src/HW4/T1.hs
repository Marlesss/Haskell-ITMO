module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import HW4.TypesUtil
import Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState func es = ES $ mapExcept (mapAnnotated func) . runES es

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ Success . (a :#)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es1 = ES $ joinExcept . mapExcept func . runES es1 where
    func :: Annotated s (ExceptState e s a) -> Except e (Annotated s a)
    func (es2 :# s2) = runES es2 s2

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState funcS = ES $ runES (wrapExceptState ()) . funcS

throwExceptState :: e -> ExceptState e s a
throwExceptState = ES . const . Error

exceptToExceptState :: Except e a -> ExceptState e s a
exceptToExceptState (Error e) = throwExceptState e
exceptToExceptState (Success a) = wrapExceptState a

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  (>>=) es func = joinExceptState (fmap func es)

data EvaluationError = DivideByZero
  deriving Show

checkedPrimApply :: (Fractional a, Eq a) => Prim a -> Except EvaluationError a
checkedPrimApply (Div _ 0) = Error DivideByZero
checkedPrimApply prim = Success $ primApply prim

evalBinary :: (Double -> Double -> Prim Double) -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
evalBinary mkPrim a b = do
    valA <- eval a
    valB <- eval b
    let t = mkPrim valA valB
    modifyExceptState (t:)
    exceptToExceptState $ checkedPrimApply t

evalUnary :: (Double -> Prim Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
evalUnary mkPrim a = do
    valA <- eval a
    let t = mkPrim valA
    modifyExceptState (t:)
    wrapExceptState (primApply t)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = wrapExceptState x
eval (Op (Add a b)) = evalBinary Add a b
eval (Op (Sub a b)) = evalBinary Sub a b
eval (Op (Mul a b)) = evalBinary Mul a b
eval (Op (Div a b)) = evalBinary Div a b
eval (Op (Abs a)) = evalUnary Abs a
eval (Op (Sgn a)) = evalUnary Sgn a
