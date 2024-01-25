module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus n Z = n
nplus n (S m) = nplus (S n) m

nmult :: N -> N -> N
nmult Z _ = Z
nmult _ Z = Z
nmult n (S m) = nplus n (nmult n m)

nsub :: N -> N -> Maybe N
nsub n Z = Just n
nsub Z (S m) = Nothing
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering
ncmp n m = case (nsub n m) of
            Nothing -> LT
            Just Z -> EQ
            Just _ -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S n)) = nEven n

nOdd :: N -> Bool
nOdd n = not $ nEven n

ndiv :: N -> N -> N
ndiv n m = case (nsub n m) of
            Nothing -> Z
            Just Z -> S Z
            Just r -> S $ ndiv r m

nmod :: N -> N -> N
nmod n m = case (nsub n m) of
            Nothing -> n
            Just Z -> Z
            Just r -> nmod r m
