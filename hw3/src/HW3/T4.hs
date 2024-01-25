module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState func state = S $ mapAnnotated func . runS state

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState st1 = S $ \s1 -> let (st2 :# s2) = runS st1 s1 in runS st2 s2

modifyState :: (s -> s) -> State s ()
modifyState funcS = S $ runS (wrapState ()) . funcS

instance Functor (State s) where
 fmap = mapState

instance Applicative (State s) where
  pure = wrapState
--  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (S stFuncRun) stA = S $ \s -> let (func :# s1) = stFuncRun s in runS (fmap func stA) s1

instance Monad (State s) where
--  (>>=) :: State s a -> (a -> State s b) -> State s b infixl 1
-- reversed implementation of join Monad
  (>>=) stA func = joinState (fmap func stA)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show


data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) x y = Op (Add x y)
  (-) x y = Op (Sub x y)
  (*) x y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  (/) x y = Op (Div x y)
  fromRational x = Val (fromRational x)

data PrimConstr a = OneArg (a -> Prim a) | MoreArg (a -> PrimConstr a)

wrapPrimConstr :: Prim a -> (PrimConstr b, [a])
wrapPrimConstr (Add a b) = (MoreArg $ \x -> OneArg $ \y -> Add x y, [a, b])
wrapPrimConstr (Sub a b) = (MoreArg $ \x -> OneArg $ \y -> Sub x y, [a, b])
wrapPrimConstr (Mul a b) = (MoreArg $ \x -> OneArg $ \y -> Mul x y, [a, b])
wrapPrimConstr (Div a b) = (MoreArg $ \x -> OneArg $ \y -> Div x y, [a, b])
wrapPrimConstr (Abs a) = (OneArg $ \x -> Abs x, [a])
wrapPrimConstr (Sgn a) = (OneArg $ \x -> Sgn x, [a])

primApply :: Fractional a => Prim a -> a
primApply (Add a b) = a + b
primApply (Sub a b) = a - b
primApply (Mul a b) = a * b
primApply (Div a b) = a / b
primApply (Abs a) = abs a
primApply (Sgn a) = signum a

evalPrim :: PrimConstr Double -> [Expr] -> State [Prim Double] Double
evalPrim (OneArg constr) [expr] = do
    val <- eval expr
    let t = constr val
    modifyState (t:)
    wrapState (primApply t)
evalPrim (MoreArg constr) (expr:ost) = do
    val <- eval expr
    evalPrim (constr val) ost
evalPrim _ _ = undefined

eval :: Expr -> State [Prim Double] Double
eval (Val x) = wrapState x
eval (Op op) = let (wrappedPrim, exprs) = wrapPrimConstr op in evalPrim wrappedPrim exprs
