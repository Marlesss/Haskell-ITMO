{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HW5.Base
  ( ArityHiFun(..)
  , mapHiFunArity
  , HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiAction(..)
  , HiMonad(..)
  ) where

import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Time.Clock
import GHC.Generics
import Codec.Serialise

data ArityHiFun =
  Various HiFun
  | Ternary HiFun
  | Binary HiFun
  | Unary HiFun

mapHiFunArity :: HiFun -> ArityHiFun
mapHiFunArity f = toArityHiFun f f where
  toArityHiFun func = case func of
    HiFunDiv -> Binary
    HiFunMul -> Binary
    HiFunAdd -> Binary
    HiFunSub -> Binary
    HiFunNot -> Unary
    HiFunAnd -> Binary
    HiFunOr -> Binary
    HiFunLessThan -> Binary
    HiFunGreaterThan -> Binary
    HiFunEquals -> Binary
    HiFunNotLessThan -> Binary
    HiFunNotGreaterThan -> Binary
    HiFunNotEquals -> Binary
    HiFunIf -> Ternary
    HiFunLength -> Unary
    HiFunToUpper -> Unary
    HiFunToLower -> Unary
    HiFunReverse -> Unary
    HiFunTrim -> Unary
    HiFunList -> Various
    HiFunRange -> Binary
    HiFunFold -> Binary
    HiFunPackBytes -> Unary
    HiFunUnpackBytes -> Unary
    HiFunEncodeUtf8 -> Unary
    HiFunDecodeUtf8 -> Unary
    HiFunZip -> Unary
    HiFunUnzip -> Unary
    HiFunSerialise -> Unary
    HiFunDeserialise -> Unary
    HiFunRead -> Unary
    HiFunWrite -> Binary
    HiFunMkDir -> Unary
    HiFunChDir -> Unary
    HiFunParseTime -> Unary
    HiFunRand -> Binary
    HiFunEcho -> Unary
    HiFunCount -> Unary
    HiFunKeys -> Unary
    HiFunValues -> Unary
    HiFunInvert -> Unary

data HiFun =
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

data HiValue =
  HiValueBool Bool
  | HiValueNumber Rational
  | HiValueNull
  | HiValueString T.Text
  | HiValueFunction HiFun
  | HiValueList (S.Seq HiValue)
  | HiValueBytes B.ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map.Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath B.ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho T.Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue