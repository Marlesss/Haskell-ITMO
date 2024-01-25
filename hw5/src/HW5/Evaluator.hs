{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module HW5.Evaluator
  ( eval
  , decodeUtf8OrNull
  ) where

import HW5.Base
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.Sequence as S
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Codec.Compression.Zlib as Zlib
import Codec.Serialise(serialise, deserialiseOrFail)
import GHC.Exts
import Data.Semigroup
import Data.Ratio
import Data.Maybe
import Data.Word(Word8)
import Data.Functor
import Data.Time.Clock
import qualified Text.Read as R
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval e = runExceptT $ evalT e

evalT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalT (HiExprValue v) = pure v
evalT (HiExprApply f args) = do
    fVal <- evalT f
    evalApply fVal args
evalT (HiExprRun e) = evalT e >>= evalRun
evalT (HiExprDict pairs) = do
  pairsVal <- traverse (\(x, y) -> do
    xVal <- evalT x
    yVal <- evalT y
    return (xVal, yVal)) pairs
  return $ HiValueDict $ Map.fromList pairsVal

evalRun :: HiMonad m => HiValue -> ExceptT HiError m HiValue
evalRun (HiValueAction a) = lift $ runAction a
evalRun _ = throwE HiErrorInvalidFunction

evalApply :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalApply (HiValueFunction f) = lazyEvalFunc (mapHiFunArity f)
evalApply (HiValueString s) = evalIndexOrSlice $ WrappedText s
evalApply (HiValueList l) = evalIndexOrSlice $ WrappedSeq l
evalApply (HiValueBytes l) = evalIndexOrSlice $ WrappedByteString l
evalApply (HiValueDict d) = \exprs -> do
  exprsVal <- traverse evalT exprs
  case exprsVal of
    [key] -> pure $ fromMaybe HiValueNull (Map.lookup key d)
    _     -> throwE HiErrorArityMismatch
evalApply _ = const $ throwE HiErrorInvalidFunction

evalIndexOrSlice :: (IndexOrSlice t, HiValuable t, HiValuable (Item t), HiMonad m) => t -> [HiExpr] -> ExceptT HiError m HiValue
evalIndexOrSlice t args = do
  argsVal <- traverse evalT args
  applyIndexOrSlice t argsVal

applyIndexOrSlice :: (IndexOrSlice t, HiValuable t, HiValuable (Item t), HiMonad m) => t -> [HiValue] -> ExceptT HiError m HiValue
applyIndexOrSlice t = \case
  [HiValueNumber i] -> do
    intI <- intOrError i
    getByIndex t intI
  [_] -> throwE HiErrorInvalidArgument
  [HiValueNull, y] -> applyIndexOrSlice t [HiValueNumber $ (toRational @Integer) 0, y]
  [x, HiValueNull] -> applyIndexOrSlice t [x, HiValueNumber . toRational . length . toList $ t]
  [HiValueNumber i, HiValueNumber j] -> do
    intI <- intOrError i
    intJ <- intOrError j
    getSlice t intI intJ
  [_, _] -> throwE HiErrorInvalidArgument
  _ -> throwE HiErrorArityMismatch

getByIndex :: (IndexOrSlice t, HiValuable (Item t), Monad m) => t -> Int -> ExceptT HiError m HiValue
getByIndex s i = case index i s of
  Nothing -> pure HiValueNull
  (Just v) -> pure $ toHiValue v

getSlice :: (IndexOrSlice t, HiValuable t, HiMonad m) => t -> Int -> Int -> ExceptT HiError m HiValue
getSlice t i j = pure . toHiValue $ slice i j t

lazyEvalFunc :: HiMonad m => ArityHiFun -> [HiExpr] -> ExceptT HiError m HiValue
lazyEvalFunc (Various f) args = evalVarious f args
lazyEvalFunc (Ternary f) [x, y, z] = evalTernary f x y z
lazyEvalFunc (Ternary _) _ = throwE HiErrorArityMismatch
lazyEvalFunc (Binary f) [x, y] = evalBinary f x y
lazyEvalFunc (Binary _) _ = throwE HiErrorArityMismatch
lazyEvalFunc (Unary f) [x] = evalUnary f x
lazyEvalFunc (Unary _) _ = throwE HiErrorArityMismatch

evalVarious :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalVarious HiFunList args = do
  argsVal <- traverse evalT args
  pure $ HiValueList $ S.fromList argsVal
evalVarious _ _ = throwE HiErrorInvalidFunction

evalTernary :: HiMonad m => HiFun -> HiExpr -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalTernary HiFunIf cond a b = do
  condVal <- evalT cond
  case condVal of
    (HiValueBool True)   -> evalT a
    (HiValueBool False)  -> evalT b
    _ -> throwE HiErrorInvalidArgument
evalTernary _ _ _ _ = throwE HiErrorInvalidFunction

evalBinary :: HiMonad m => HiFun -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalBinary HiFunAnd x y = do
  xVal <- evalT x
  case xVal of
    HiValueBool False -> pure xVal
    HiValueNull       -> pure xVal
    _                 -> evalT y
evalBinary HiFunOr x y = do
  xVal <- evalT x
  case xVal of
    HiValueBool False -> evalT y
    HiValueNull       -> evalT y
    _                 -> pure xVal
evalBinary f x y = do
  xVal <- evalT x
  yVal <- evalT y
  applyBinary f xVal yVal

applyBinary :: HiMonad m => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
applyBinary HiFunDiv _ (HiValueNumber 0) = throwE HiErrorDivideByZero
applyBinary HiFunDiv (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x / y
applyBinary HiFunDiv (HiValueString x) (HiValueString y) = pure $ HiValueString $ T.intercalate (T.pack "/") [x, y]
applyBinary HiFunMul (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x * y
applyBinary HiFunMul (HiValueString x) (HiValueNumber y) = pure $ HiValueString $ stimes (truncate y :: Integer) x
applyBinary HiFunMul (HiValueList x) (HiValueNumber y) = pure $ HiValueList $ stimes (truncate y :: Integer) x
applyBinary HiFunMul (HiValueBytes x) (HiValueNumber y) = pure $ HiValueBytes $ stimes (truncate y :: Integer) x
applyBinary HiFunAdd (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x + y
applyBinary HiFunAdd (HiValueString x) (HiValueString y) = pure $ HiValueString $ x <> y
applyBinary HiFunAdd (HiValueList x) (HiValueList y) = pure $ HiValueList $ x <> y
applyBinary HiFunAdd (HiValueBytes x) (HiValueBytes y) = pure $ HiValueBytes $ x <> y
applyBinary HiFunAdd (HiValueTime x) (HiValueNumber y) = pure $ HiValueTime $ addUTCTime (fromRational y) x
applyBinary HiFunSub (HiValueNumber x) (HiValueNumber y) = pure $ HiValueNumber $ x - y
applyBinary HiFunSub (HiValueTime x) (HiValueTime y) = pure $ HiValueNumber $ toRational $ diffUTCTime x y
applyBinary HiFunLessThan x y = pure $ HiValueBool $ x < y
applyBinary HiFunGreaterThan x y = pure $ HiValueBool $ x > y
applyBinary HiFunEquals x y = pure $ HiValueBool $ x == y
applyBinary HiFunNotLessThan x y = pure $ HiValueBool $ x >= y
applyBinary HiFunNotGreaterThan x y = pure $ HiValueBool $ x <= y
applyBinary HiFunNotEquals x y = pure $ HiValueBool $ x /= y
applyBinary HiFunRange (HiValueNumber x) (HiValueNumber y) = pure $ HiValueList $ S.fromList $ map HiValueNumber [x..y]
applyBinary HiFunFold _ (HiValueList S.Empty) = throwE HiErrorInvalidArgument
applyBinary HiFunFold (HiValueFunction f) (HiValueList l) = case mapHiFunArity f of
                                                              (Binary _) -> foldl1 (\x y -> do
                                                                                        xVal <- x
                                                                                        yVal <- y
                                                                                        applyBinary f xVal yVal) (pure <$> l)
                                                              _          -> throwE HiErrorInvalidArgument
applyBinary HiFunWrite (HiValueString f) (HiValueString s) = pure $ HiValueAction $ HiActionWrite (T.unpack f) (T.Encoding.encodeUtf8 s)
applyBinary HiFunRand (HiValueNumber x) (HiValueNumber y) = HiValueAction <$> (HiActionRand <$> intOrError x <*> intOrError y)
applyBinary _ _ _ = throwE HiErrorInvalidArgument

evalUnary :: HiMonad m => HiFun -> HiExpr -> ExceptT HiError m HiValue
evalUnary f x = do
  xVal <- evalT x
  applyUnary f xVal

applyUnary :: HiMonad m => HiFun -> HiValue -> ExceptT HiError m HiValue
applyUnary HiFunNot (HiValueBool x) = pure $ HiValueBool $ not x
applyUnary HiFunLength (HiValueString x) = pure $ HiValueNumber $ toRational . T.length $ x
applyUnary HiFunLength (HiValueList l) = pure $ HiValueNumber $ toRational . S.length $ l
applyUnary HiFunToUpper (HiValueString x) = pure $ HiValueString $ T.toUpper x
applyUnary HiFunToLower (HiValueString x) = pure $ HiValueString $ T.toLower x
applyUnary HiFunReverse (HiValueString x) = pure $ HiValueString $ T.reverse x
applyUnary HiFunReverse (HiValueList l) = pure $ HiValueList $ S.reverse l
applyUnary HiFunTrim (HiValueString x) = pure $ HiValueString $ T.strip x
applyUnary HiFunPackBytes (HiValueList l) = mapM hiValueToWord8 (toList $ WrappedSeq l) <&> HiValueBytes . B.pack
applyUnary HiFunUnpackBytes (HiValueBytes l) =  pure $ HiValueList $ S.fromList $ map word8ToHiValue (B.unpack l)
applyUnary HiFunEncodeUtf8 (HiValueString t) = pure $ HiValueBytes $ T.Encoding.encodeUtf8 t
applyUnary HiFunDecodeUtf8 (HiValueBytes l) = pure $ fromMaybe HiValueNull $ decodeUtf8OrNull l
applyUnary HiFunZip (HiValueBytes l) = pure $ HiValueBytes $ zipBS l
applyUnary HiFunUnzip (HiValueBytes l) = pure $ HiValueBytes $ unzipBS l
applyUnary HiFunSerialise v = pure $ HiValueBytes $ toStrict $ serialise v
applyUnary HiFunDeserialise (HiValueBytes l) = deserialiseHiValue l
applyUnary HiFunRead (HiValueString s) = pure $ HiValueAction $ HiActionRead $ T.unpack s
applyUnary HiFunMkDir (HiValueString s) = pure $ HiValueAction $ HiActionMkDir $ T.unpack s
applyUnary HiFunChDir (HiValueString s) = pure $ HiValueAction $ HiActionChDir $ T.unpack s
applyUnary HiFunParseTime (HiValueString s) = pure $ maybe HiValueNull HiValueTime $ R.readMaybe $ T.unpack s
applyUnary HiFunEcho (HiValueString s) = pure $ HiValueAction $ HiActionEcho s
applyUnary HiFunCount (HiValueString s) = count $ WrappedText s
applyUnary HiFunCount (HiValueList s) = count $ WrappedSeq s
applyUnary HiFunCount (HiValueBytes s) = count $ WrappedByteString s
applyUnary HiFunKeys (HiValueDict d) = pure $ HiValueList $ S.fromList $ Map.keys d
applyUnary HiFunValues (HiValueDict d) = pure $ HiValueList $ S.fromList $ Map.elems d
applyUnary HiFunInvert (HiValueDict d) = pure $ HiValueDict $ Map.map (HiValueList . S.fromList) $ foldr (\(k, v) m -> Map.insertWith (++) v [k] m) Map.empty $ Map.toList d
applyUnary _ _ = throwE HiErrorInvalidArgument

count :: (IsList t, HiValuable (Item t), HiMonad m) => t -> ExceptT HiError m HiValue
count s = pure $ HiValueDict $ Map.map (HiValueNumber . (toRational @Integer)) $ foldr ((\k m -> Map.insertWith (+) k 1 m) . toHiValue) Map.empty $ toList s

deserialiseHiValue :: HiMonad m => B.ByteString -> ExceptT HiError m HiValue
deserialiseHiValue b = case deserialiseOrFail $ fromStrict b of
  (Left _) -> throwE HiErrorInvalidArgument
  (Right v) -> pure v

zipBS :: B.ByteString -> B.ByteString
zipBS = toStrict . Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.bestCompression}) . fromStrict

unzipBS :: B.ByteString -> B.ByteString
unzipBS = toStrict . Zlib.decompress . fromStrict

decodeUtf8OrNull :: B.ByteString -> Maybe HiValue
decodeUtf8OrNull l = case T.Encoding.decodeUtf8' l of
  (Left _) -> Nothing
  (Right t) -> Just $ HiValueString t

hiValueToWord8 :: HiMonad m => HiValue -> ExceptT HiError m Word8
hiValueToWord8 (HiValueNumber x) = do
  intX <- intOrError x
  if 0 <= intX && intX < 256
    then pure $ fromIntegral intX
    else throwE HiErrorInvalidArgument
hiValueToWord8 _ = throwE HiErrorInvalidArgument

word8ToHiValue :: Word8 -> HiValue
word8ToHiValue w = HiValueNumber $ (toRational @Integer) $ fromIntegral w

intOrError :: HiMonad m => Rational -> ExceptT HiError m Int
intOrError r | denominator r == 1 = pure $ fromInteger $ numerator r
             | otherwise          = throwE HiErrorInvalidArgument

class IsList t => IndexOrSlice t where
  index :: Int -> t -> Maybe (Item t)
  index i t | i < 0                  = Nothing
            | i >= length (toList t) = Nothing
            | otherwise              = Just $ toList t !! i

  slice :: Int -> Int -> t -> t
  slice i j c = let i' = shiftNegative i
                    j' = shiftNegative j in
                fromList $ take (j' - i') $ drop i' $ toList c where
                  shiftNegative x | x >= 0    = x
                                  | otherwise = x + length (toList c)

class HiValuable a where
  toHiValue :: a -> HiValue

instance HiValuable HiValue where
  toHiValue = id

newtype WrappedText = WrappedText T.Text

instance HiValuable WrappedText where
  toHiValue (WrappedText t) = HiValueString t

instance IsList WrappedText where
  type Item WrappedText = Char
  fromList list = WrappedText $ fromList list
  toList (WrappedText t) = toList t

instance IndexOrSlice WrappedText

instance HiValuable Char where
  toHiValue = HiValueString . T.pack . (:[])

instance HiValuable Word8 where
  toHiValue = word8ToHiValue

newtype WrappedSeq a = WrappedSeq (S.Seq a)

instance HiValuable (WrappedSeq HiValue) where
  toHiValue (WrappedSeq s) = HiValueList s

instance IsList (WrappedSeq a) where
  type Item (WrappedSeq a) = a
  fromList = WrappedSeq . fromList
  toList (WrappedSeq s) = toList s

instance IndexOrSlice (WrappedSeq a)

newtype WrappedByteString = WrappedByteString B.ByteString

instance HiValuable WrappedByteString where
  toHiValue (WrappedByteString b) = HiValueBytes b

instance IsList WrappedByteString where
  type Item WrappedByteString = Word8
  fromList = WrappedByteString . B.pack
  toList (WrappedByteString b) = B.unpack b

instance IndexOrSlice WrappedByteString