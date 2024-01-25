{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  , pDouble
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Scientific

import HW4.Types
import HW4.T1


data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P parser) input = case runES parser (0, input) of
    Error e -> Error e
    Success (a :# _) -> Success a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ Error . ErrorAtPos . fst

instance Alternative Parser where
  empty = parseError
  (<|>) (P es1) (P es2) = P $ ES $ \inp -> case runES es1 inp of
    Error _ -> runES es2 inp
    r@(Success _) -> r

-- No metohds
instance MonadPlus Parser


getBinaryPrim :: Char -> a -> a -> Prim a
getBinaryPrim '+' = Add
getBinaryPrim '-' = Sub
getBinaryPrim '*' = Mul
getBinaryPrim '/' = Div
getBinaryPrim _ = undefined

--E->TE'
--E'->+/- TE'
--E'-> eps
--T->FT'
--T'->*//FT'
--T'->eps
--F->(E)
--F->n
parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr

pExpr :: Parser Expr
pExpr = pWS *> pE <* pWS <* pEof

pE :: Parser Expr
pE = do
  t <- pT
  pEPrime t

pEPrime :: Expr -> Parser Expr
pEPrime acc = pTOp acc <|> pure acc

pTOp :: Expr -> Parser Expr
pTOp acc = do
  op <- pWS *> (pCharEq '+' <|> pCharEq '-') <* pWS
  right <- pT
  pEPrime $ Op $ getBinaryPrim op acc right

pT :: Parser Expr
pT = do
  f <- pF
  pTPrime f

pTPrime :: Expr -> Parser Expr
pTPrime acc = pFOp acc <|> pure acc

pFOp :: Expr -> Parser Expr
pFOp acc = do
  op <- pWS *> (pCharEq '*' <|> pCharEq '/') <* pWS
  right <- pF
  pTPrime $ Op $ getBinaryPrim op acc right

pF :: Parser Expr
pF = pNum <|> (pCharEq '(' *> pE <* pCharEq ')')

pNum :: Parser Expr
pNum = do
  val <- pDouble <|> pInt
  pure $ Val val

pDigitString :: Parser String
pDigitString = some $ pCharPred isDigit

digitStringToNum :: Num b => [Char] -> [b]
digitStringToNum = map (fromIntegral . digitToInt)

digitsToNum :: Num b => [b] -> b
digitsToNum = foldl (\acc digit -> acc * 10 + digit) 0

pInt :: Parser Double
pInt = do
  integerString <- pDigitString
  let digitArr = digitStringToNum integerString
  let integer = digitsToNum digitArr
  pure integer

pDouble :: Parser Double
pDouble = do
  wholeString <- pDigitString
  _ <- pCharEq '.'
  fractionalString <- pDigitString
  let mantissa = digitsToNum $ digitStringToNum (wholeString ++ fractionalString)
  pure $ toRealFloat $ scientific mantissa (-length fractionalString)

pWS :: Parser [Char]
pWS = many $ pCharPred isSpace

pCharEq :: Char -> Parser Char
pCharEq ch = pCharPred (ch==)

pCharPred :: (Char -> Bool) -> Parser Char
pCharPred p = mfilter p pChar

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, []))
    _  -> Error (ErrorAtPos pos)


