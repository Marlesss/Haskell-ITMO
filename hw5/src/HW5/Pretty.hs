{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase #-}

module HW5.Pretty
  ( prettyValue
  , prettyParseErrorBundle
  , putDocLn
  , prettyPermissionException
  , prettyHiError
  ) where

import HW5.Base
import HW5.Action(PermissionException(..), HiPermission(..))

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Data.Map as Map
import Data.Void (Void)
import Data.Ratio
import Data.Word(Word8)
import Data.Time.Clock
import Data.Foldable
import Numeric
import Prettyprinter hiding (prettyList)
import Prettyprinter.Render.Terminal
import Text.Megaparsec hiding (parse)

data HiValueAnn =
    HiValueBoolAnn
    | HiValueNumberAnn
    | HiValueNullAnn
    | HiValueStringAnn
    | HiValueFunctionAnn
    | HiValueListAnn
    | HiValueBytesAnn
    | HiValueActionAnn
    | HiValueTimeAnn
    | HiValueDictAnn

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue v = reAnnotate hiAnnotate $ prettyHiValueAnn v

hiAnnotate :: HiValueAnn -> AnsiStyle
hiAnnotate HiValueBoolAnn = colorDull Yellow
hiAnnotate HiValueNumberAnn = colorDull Blue
hiAnnotate HiValueNullAnn = colorDull Yellow
hiAnnotate HiValueStringAnn = colorDull Green
hiAnnotate HiValueFunctionAnn = colorDull Yellow
hiAnnotate HiValueListAnn = colorDull Cyan
hiAnnotate HiValueBytesAnn = colorDull Magenta
hiAnnotate HiValueActionAnn = colorDull Yellow
hiAnnotate HiValueTimeAnn = colorDull Blue
hiAnnotate HiValueDictAnn = colorDull Cyan

prettyHiValueAnn :: HiValue -> Doc HiValueAnn
prettyHiValueAnn = \case
  (HiValueFunction f) -> prettyHiFun f
  (HiValueNumber v)   -> prettyHiValueNumber v
  (HiValueBool v)     -> prettyHiValueBool v
  HiValueNull         -> prettyNull
  (HiValueString v)   -> prettyString v
  (HiValueList v)     -> prettyList v
  (HiValueBytes v)    -> prettyByteString v
  (HiValueAction f)   -> prettyHiAction f
  (HiValueTime t)     -> prettyTime t
  (HiValueDict m)     -> prettyDict m

prettyHiFun :: HiFun -> Doc HiValueAnn
prettyHiFun f = annotate HiValueFunctionAnn $ (pretty @String) $ case f of
  HiFunDiv -> "div"
  HiFunMul -> "mul"
  HiFunSub -> "sub"
  HiFunAdd -> "add"
  HiFunNot -> "not"
  HiFunAnd -> "and"
  HiFunOr  -> "or"
  HiFunLessThan -> "less-than"
  HiFunGreaterThan -> "greater-than"
  HiFunEquals -> "equals"
  HiFunNotLessThan -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals -> "not-equals"
  HiFunIf -> "if"
  HiFunLength -> "length"
  HiFunToUpper -> "to-upper"
  HiFunToLower -> "to-lower"
  HiFunReverse -> "reverse"
  HiFunTrim -> "trim"
  HiFunList -> "list"
  HiFunRange -> "range"
  HiFunFold -> "fold"
  HiFunPackBytes -> "pack-bytes"
  HiFunUnpackBytes -> "unpack-bytes"
  HiFunEncodeUtf8 -> "encode-utf8"
  HiFunDecodeUtf8 -> "decode-utf8"
  HiFunZip -> "zip"
  HiFunUnzip -> "unzip"
  HiFunSerialise -> "serialise"
  HiFunDeserialise -> "deserialise"
  HiFunRead -> "read"
  HiFunWrite -> "write"
  HiFunMkDir -> "mkdir"
  HiFunChDir -> "cd"
  HiFunParseTime -> "parse-time"
  HiFunRand -> "rand"
  HiFunEcho -> "echo"
  HiFunCount -> "count"
  HiFunKeys -> "keys"
  HiFunValues -> "values"
  HiFunInvert -> "invert"

prettyHiValueNumber :: Rational -> Doc HiValueAnn
prettyHiValueNumber r = annotate HiValueNumberAnn $ if denominator r == 1
  then pretty $ numerator r
  else prettyRational r

prettyRational :: Rational -> Doc a
prettyRational r = if recDiv (recDiv (denominator r) 2) 5 == 1
  then (pretty @String) $ (showFFloat @Double) Nothing (fromRational r) ""
  else prettyFraction r where
    recDiv x y = if x `mod` y == 0
      then recDiv (x `div` y) y
      else x

prettyFraction :: Rational -> Doc a
prettyFraction r = let (n, d) = (numerator r, denominator r)
                       (quotR, remR) = n `quotRem` d in
  if quotR /= 0
  then pretty quotR <+> (pretty @String) (if r > 0 then "+" else "-") <+> prettyFraction (abs remR % d)
  else pretty remR <> slash <> pretty d

prettyHiValueBool :: Bool -> Doc HiValueAnn
prettyHiValueBool v = annotate HiValueBoolAnn $ (pretty @String) $ if v then "true" else "false"

prettyByteString :: B.ByteString -> Doc HiValueAnn
prettyByteString b = annotate HiValueBytesAnn $ encloseSep ((pretty @String) "[# ") ((pretty @String) " #]") space (map (pretty . WrappedWord8) (B.unpack b))

newtype WrappedWord8 = WrappedWord8 Word8

instance Pretty WrappedWord8 where
  pretty (WrappedWord8 w) = (pretty @String) $ (if w < 16 then "0" else "") ++ showHex w ""

prettyHiAction :: HiAction -> Doc HiValueAnn
prettyHiAction a = annotate HiValueActionAnn $ case a of
  (HiActionRead fp) -> (pretty @String) "read" <> lparen <> viaShow fp <> rparen
  (HiActionWrite fp bs) -> (pretty @String) "read" <> lparen <> viaShow fp <> comma <+> prettyByteString bs <> rparen
  (HiActionMkDir fp) -> (pretty @String) "mkdir" <> lparen <> viaShow fp <> rparen
  (HiActionChDir fp) -> (pretty @String) "cd" <> lparen <> viaShow fp <> rparen
  HiActionCwd -> (pretty @String) "cwd"
  HiActionNow -> (pretty @String) "now"
  (HiActionRand x y) -> (pretty @String) "rand" <> lparen <> (pretty @Int) x <> comma <+> (pretty @Int) y <> rparen
  (HiActionEcho s) -> (pretty @String) "echo" <> lparen <> viaShow s <> rparen

prettyDict :: Map.Map HiValue HiValue -> Doc HiValueAnn
prettyDict m = annotate HiValueDictAnn $ encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) (map (\(k, v) -> prettyHiValueAnn k <> colon <+> prettyHiValueAnn v) $ Map.toList m)

prettyList :: S.Seq HiValue -> Doc HiValueAnn
prettyList v = annotate HiValueListAnn $ encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map prettyHiValueAnn $ toList v)

prettyTime :: UTCTime -> Doc HiValueAnn
prettyTime t = annotate HiValueTimeAnn $ viaShow t

prettyNull :: Doc HiValueAnn
prettyNull = annotate HiValueNullAnn $ (pretty @String) "null"

prettyString :: T.Text -> Doc HiValueAnn
prettyString v = annotate HiValueStringAnn $ viaShow v

prettyParseErrorBundle :: ParseErrorBundle String Void -> Doc AnsiStyle
prettyParseErrorBundle p = annotate (color Red) $ pretty $ errorBundlePretty p

prettyPermissionException :: PermissionException -> Doc AnsiStyle
prettyPermissionException p = annotate (color Red) $ case p of
  (PermissionRequired permission) -> (pretty @String) "Required permission " <> prettyHiPermission permission

prettyHiPermission :: HiPermission -> Doc a
prettyHiPermission permission = (pretty @String) $ case permission of
  AllowRead -> "read"
  AllowWrite -> "write"
  AllowTime -> "time"

prettyHiError :: HiError -> Doc AnsiStyle
prettyHiError e = annotate (color Red) $ (pretty @String) $ "Evaluation error: " ++ case e of
  HiErrorInvalidArgument -> "invalid argument"
  HiErrorInvalidFunction -> "invalid function"
  HiErrorArityMismatch   -> "wrong count of arguments"
  HiErrorDivideByZero    -> "division by zero"

putDocLn :: Doc AnsiStyle -> IO ()
putDocLn d = putDoc d >> putStrLn ""