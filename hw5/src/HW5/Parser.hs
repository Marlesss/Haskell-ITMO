module HW5.Parser
  ( parse ) where

import Data.Char
import Data.List(intercalate)
import Data.Void (Void)
import Control.Monad
import Control.Monad.Combinators.Expr
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.ByteString as B
import qualified Text.Read as R
import Data.Word(Word8)

import HW5.Base

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (hidden space *> exprParser <* eof) ""

exprParser :: Parser HiExpr
exprParser = makeExprParser pHiExpr operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ postfix (symbol "!") HiExprRun ]
  , [ infixL (symbol "*") $ applyHiFun HiFunMul
    , infixL ((lexeme . try) (string "/" <* notFollowedBy (string "="))) $ applyHiFun HiFunDiv ] -- 7
  , [ infixL (symbol "+") $ applyHiFun HiFunAdd
    , infixL (symbol "-") $ applyHiFun HiFunSub ] -- 6
  , [ infixN (symbol "<=") $ applyHiFun HiFunNotGreaterThan
    , infixN (symbol "<") $ applyHiFun HiFunLessThan
    , infixN (symbol ">=") $ applyHiFun HiFunNotLessThan
    , infixN (symbol ">") $ applyHiFun HiFunGreaterThan
    , infixN (symbol "==") $ applyHiFun HiFunEquals
    , infixN (symbol "/=") $ applyHiFun HiFunNotEquals ] -- 4
  , [ infixR (symbol "&&") $ applyHiFun HiFunAnd ] -- 3
  , [ infixR (symbol "||") $ applyHiFun HiFunOr ] -- 2
  ]

applyHiFun :: HiFun -> HiExpr -> HiExpr -> HiExpr
applyHiFun f x y = HiExprApply (HiExprValue $ HiValueFunction f) [x, y]

postfix :: Parser String -> (HiExpr -> HiExpr) -> Operator Parser HiExpr
postfix pName f = Postfix (f <$ pName)

infixL :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixL pName f = InfixL  (f <$ pName)

infixR :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixR pName f = InfixR  (f <$ pName)

infixN :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixN pName f = InfixN  (f <$ pName)

pHiExpr :: Parser HiExpr
pHiExpr = pTerm >>= pRecursiveChoice [ pParensApply, pDotApply ]

pTerm :: Parser HiExpr
pTerm = choice
  [ parens exprParser <?> "expression in parentheses"
  , pHiExprValue ]

pRecursiveChoice :: [HiExpr -> Parser HiExpr] -> HiExpr -> Parser HiExpr
pRecursiveChoice parsers e = choice (map (\p -> p e >>= pRecursiveChoice parsers) parsers) <|> pure e

pParensApply :: HiExpr -> Parser HiExpr
pParensApply f = HiExprApply f <$> parens pHiExprSequence

pDotApply :: HiExpr -> Parser HiExpr
pDotApply f = (\s -> HiExprApply f [HiExprValue $ HiValueString $ T.pack s]) <$> lexeme (string "." *> pDotKey)

pDotKey :: Parser String
pDotKey = intercalate "-" <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

pSequence :: Parser a -> Parser [a]
pSequence p = p `sepBy` symbol ","

pHiExprSequence :: Parser [HiExpr]
pHiExprSequence = pSequence exprParser

pHiExprPair :: Parser (HiExpr, HiExpr)
pHiExprPair = do
  first <- exprParser
  void (symbol ":")
  second <- exprParser
  return (first, second)

pHiExprValue :: Parser HiExpr
pHiExprValue = lexeme $ (HiExprValue <$> pHiValue) <|> pHiList <|> pHiDict

pHiList :: Parser HiExpr
pHiList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> between (symbol "[") (symbol "]") pHiExprSequence <?> "list"

pHiDict :: Parser HiExpr
pHiDict = HiExprDict <$> between (symbol "{") (symbol "}") (pSequence pHiExprPair) <?> "dictionary"

pHiValue :: Parser HiValue
pHiValue = choice
  [ pHiValueNumber
  , pHiValueBool
  , pHiValueFunction
  , pHiValueNull
  , pHiValueString
  , pHiValueBytes
  , pHiAction ]

pHiValueNumber :: Parser HiValue
pHiValueNumber = HiValueNumber . toRational <$> L.signed sc L.scientific <?> "number"

pHiValueBool :: Parser HiValue
pHiValueBool = HiValueBool <$> choice
  [ True <$ string "true"
  , False <$ string "false" ] <?> "boolean"

pHiValueFunction :: Parser HiValue
pHiValueFunction = HiValueFunction <$> choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"
  , HiFunAnd <$ string "and"
  , HiFunOr <$ string "or"
  , HiFunLessThan <$ string "less-than"
  , HiFunGreaterThan <$ string "greater-than"
  , HiFunEquals <$ string "equals"
  , HiFunNotLessThan <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals <$ string "not-equals"
  , HiFunNot <$ string "not"
  , HiFunIf <$ string "if"
  , HiFunLength <$ string "length"
  , HiFunToUpper <$ string "to-upper"
  , HiFunToLower <$ string "to-lower"
  , HiFunReverse <$ string "reverse"
  , HiFunTrim <$ string "trim"
  , HiFunList <$ string "list"
  , HiFunRange <$ string "range"
  , HiFunFold <$ string "fold"
  , HiFunPackBytes <$ string "pack-bytes"
  , HiFunUnpackBytes <$ string "unpack-bytes"
  , HiFunEncodeUtf8 <$ string "encode-utf8"
  , HiFunDecodeUtf8 <$ string "decode-utf8"
  , HiFunZip <$ string "zip"
  , HiFunUnzip <$ string "unzip"
  , HiFunSerialise <$ string "serialise"
  , HiFunDeserialise <$ string "deserialise"
  , HiFunRead <$ string "read"
  , HiFunWrite <$ string "write"
  , HiFunMkDir <$ string "mkdir"
  , HiFunChDir <$ string "cd"
  , HiFunParseTime <$ string "parse-time"
  , HiFunRand <$ string "rand"
  , HiFunEcho <$ string "echo"
  , HiFunCount <$ string "count"
  , HiFunKeys <$ string "keys"
  , HiFunValues <$ string "values"
  , HiFunInvert <$ string "invert" ] <?> "function"

pHiValueNull :: Parser HiValue
pHiValueNull = HiValueNull <$ string "null" <?> "null"

pHiValueString :: Parser HiValue
pHiValueString = HiValueString . T.pack <$> stringLiteral <?> "string"

pHiValueBytes :: Parser HiValue
pHiValueBytes = HiValueBytes . B.pack <$> between (symbol "[#") (symbol "#]") (pWord8 `sepEndBy` space1) <?> "byte string"

pWord8 :: Parser Word8
pWord8 = label "two-hex-digit number" $ do
  c1 <- hexDigitChar
  c2 <- hexDigitChar
  case R.readMaybe ("0x" ++ [c1, c2]) of
    Nothing  -> fail "Invalid two-digit hex value"
    (Just v) -> pure v

pHiAction :: Parser HiValue
pHiAction = HiValueAction <$> choice
  [ HiActionCwd <$ string "cwd"
  , HiActionNow <$ string "now" ] <?> "action"

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

parens :: Parser a -> Parser a
parens p = lexeme $ between (symbol "(") (symbol ")") p
