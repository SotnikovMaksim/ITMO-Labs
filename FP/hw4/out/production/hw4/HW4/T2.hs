{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..),
    runP,
    pChar,
    parseError,
    parseExpr,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus, mfilter, void)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Scientific (scientific, toRealFloat)
import GHC.Natural (Natural)
import HW4.T1 (ExceptState (..))
import HW4.Types

-- | Represents an error that occurs at a specific position in the input.
data ParseError = ErrorAtPos Natural -- HLint suggests to use 'newtype' instead of 'data'
  deriving (Show, Eq)

-- | A parser that operates on a string and maintains the current position.
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

instance Alternative Parser where
  empty = parseError
  P leftParser <|> P rightParser = P $ ES $ \state ->
    case runES leftParser state of
      success@(Success _) -> success
      (Error _)           -> runES rightParser state

instance MonadPlus Parser

-- | Runs a parser on a given input string.
runP :: Parser a -> String -> Except ParseError a
runP (P parser) input =
  case runES parser (0, input) of
    Error parseErr        -> Error parseErr
    Success (result :# _) -> Success result

-- | Parses an arithmetic expression from a string.
parseExpr :: String -> Except ParseError Expr
parseExpr = runP (strip parseExprInternal <* parseEnd)

-- | Throws a parse error at the current position.
parseError :: Parser a
parseError = P (ES $ \(position, _) -> Error (ErrorAtPos position))

-- Internal parser implementations

parseExprInternal :: Parser Expr
parseExprInternal = do
  termExpr <- strip parseTerm
  tryToExtend extendExpr termExpr

extendExpr :: Expr -> Parser Expr
extendExpr initialTerm = do
  operator <- strip (parseChar '+' <|> parseChar '-')
  nextTerm <- strip parseTerm
  case operator of
    '+' -> tryToExtend extendExpr (Op (Add initialTerm nextTerm))
    '-' -> tryToExtend extendExpr (Op (Sub initialTerm nextTerm))
    _   -> parseError

parseTerm :: Parser Expr
parseTerm = do
  factorExpr <- strip parseFactor
  tryToExtend extendTerm factorExpr

extendTerm :: Expr -> Parser Expr
extendTerm initialFactor = do
  operator <- strip (parseChar '*' <|> parseChar '/')
  nextFactor <- strip parseFactor
  case operator of
    '*' -> tryToExtend extendTerm (Op (Mul initialFactor nextFactor))
    '/' -> tryToExtend extendTerm (Op (Div initialFactor nextFactor))
    _ -> parseError

parseFactor :: Parser Expr
parseFactor = strip (parseNumber <|> parseExprInParenthesis)
  where
    parseExprInParenthesis = parseChar '(' *> parseExprInternal <* parseChar ')'

parseEnd :: Parser ()
parseEnd = P $ ES $ \(position, remainingStr) ->
  case remainingStr of
    []      -> Success (() :# (position, remainingStr))
    (_ : _) -> Error $ ErrorAtPos position

-- Helper functions

-- | Parses a single character from the input.
pChar :: Parser Char
pChar = P $ ES $ \(position, inputStr) ->
  case inputStr of
    []                    -> Error (ErrorAtPos position)
    (char : remainingStr) -> Success (char :# (position + 1, remainingStr))

-- | Parses a specific character, failing if the character doesn't match.
parseChar :: Char -> Parser Char
parseChar expected = mfilter (== expected) pChar

-- | Parses and ignores a specific character.
skipChar :: Char -> Parser ()
skipChar character = void (parseChar character)

-- | Strips leading and trailing whitespace from a parsed result.
strip :: Parser a -> Parser a
strip parser = parseWhitespace *> parser <* parseWhitespace
  where
    parseWhitespace = many (mfilter isSpace pChar)

-- | Parses a floating-point number.
parseNumber :: Parser Expr
parseNumber = do
  integerPart    <- parseDigits; skipChar '.'
  fractionalPart <- parseDigits
  let fractionalLength = -length fractionalPart
  pure $
    Val $
      toRealFloat $
        scientific (parseInteger (integerPart ++ fractionalPart)) fractionalLength
  where
    parseInteger = foldl (\acc digit -> acc * 10 + toInteger (digitToInt digit)) 0

-- | Parses digits into a string.
parseDigits :: Parser String
parseDigits = some (mfilter isDigit pChar)

-- | Tries to extend a parsed expression with additional terms or factors.
tryToExtend :: (Expr -> Parser Expr) -> Expr -> Parser Expr
tryToExtend parser expr = parser expr <|> pure expr
