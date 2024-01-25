module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , mergeList
  , distFun
  , wrapFun
  , unwrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# ea, b :# eb) = (a, b) :# ea <> eb

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success a, Success b) = Success (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, b) = High (a, unwrapPrioritised b)
distPrioritised (a, High b) = High (unwrapPrioritised a, b)
distPrioritised (Medium a, b) = Medium (a, unwrapPrioritised b)
distPrioritised (a, Medium b) = Medium (unwrapPrioritised a, b)
distPrioritised (Low a, Low b) = Low (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

unwrapPrioritised :: Prioritised a -> a
unwrapPrioritised (High a) = a
unwrapPrioritised (Medium a) = a
unwrapPrioritised (Low a) = a


distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> sa, b :> sb) = (a, b) :> distStream (sa, sb)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (a :. tailA, listB) = tupled listB where
    tupled Nil = distList (tailA, listB)
    tupled (b :. tailB) = (a, b) :. tupled tailB

wrapList :: a -> List a
wrapList a = a :. Nil

mergeList :: List a -> List a -> List a
mergeList Nil b = b
mergeList (a :. tailA) b = a :. mergeList tailA b

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F fa, F fb) = F $ \i -> (fa i, fb i)

wrapFun :: a -> Fun i a
wrapFun a = F (\_ -> a)

unwrapFun :: Fun i a -> (i -> a)
unwrapFun (F f) = f

