module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
--doubleNeg :: a -> Not a -> Void
--doubleNeg :: a -> (a -> Void) -> Void
doubleNeg a notA = notA a

reduceTripleNeg :: Not (Not (Not a)) -> Not a
--reduceTripleNeg :: (Not (Not a) -> Void) -> Not a
--reduceTripleNeg :: ((Not a -> Void) -> Void) -> Not a
--reduceTripleNeg :: (((a -> Void) -> Void) -> Void) -> Not a
reduceTripleNeg notNotA = notNotA . doubleNeg
