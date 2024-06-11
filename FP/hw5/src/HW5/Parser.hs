module HW5.Parser
  ( parse,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as DB
import qualified Data.Text as DT
import Data.Void
import Data.Word (Word8)
import HW5.Base
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)

type Parser = Parsec Void String

-------------------- [ CORE PARSING ] --------------------

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (parseExpression <* eof) ""

----------------- [ EXPRESSION PARSING ] -----------------

-- IMHO using '*> / <*' is much more readable than 'between'
-- | Parses a 'HiExpr' expression, handling operator precedence and whitespace.
parseExpression :: Parser HiExpr
parseExpression = makeExprParser (skipWhitespaces *> parseInnerExpression <* skipWhitespaces) operatorPrecedenceTable

-- | Parses an inner expression in the language, including lists, dictionaries, and parenthesized expressions.
parseInnerExpression :: Parser HiExpr
parseInnerExpression = do
  parsedExpr   <- parseBaseExpression
  args         <- parseArguments
  actionSymbol <- optional $ char '!'
  return $
    let adjustedExpr = foldl HiExprApply parsedExpr args
      in case actionSymbol of
          (Just _) -> HiExprRun adjustedExpr
          Nothing  -> adjustedExpr

-- | Parses basic expressions including numbers, strings, lists, and dictionaries.
parseBaseExpression :: Parser HiExpr
parseBaseExpression =
  choice [
      parseValueExpression,
      parseListExpression,
      parseDictionaryExpression,
      inParentheses parseExpression
    ]
    
-- | Parses an arguments which might be represented as (A, B, ...) or '.A'
parseArguments :: Parser [[HiExpr]]
parseArguments = many (inParentheses parseFunctionArguments <|> parseDotAccess)

-- | Parses a value and wraps it in a 'HiExpr'.
parseValueExpression :: Parser HiExpr
parseValueExpression = skipWhitespaces *> (HiExprValue <$> parseValue)

-- | Parses a comma-separated list of expressions as function arguments.
parseFunctionArguments :: Parser [HiExpr]
parseFunctionArguments = skipWhitespaces *> sepBy parseExpression (char ',')

-- | Parses dot-access expressions used for key access in dictionaries.
parseDotAccess :: Parser [HiExpr]
parseDotAccess = char '.' >> parseIdentifierExpression `sepBy1` char '-'
  where
    parseIdentifierExpression = HiExprValue . HiValueString . DT.pack <$> parseIdentifier
    parseIdentifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

------------------- [ OPERATORS TABLE ] ------------------

-- | Defines the operator precedence table for parsing expressions.
operatorPrecedenceTable :: [[Operator Parser HiExpr]]
operatorPrecedenceTable =
  [ -- Multiplication and division with highest precedence
    [ infixLeft "*" HiFunMul,
      divisionOperator "/"
    ],
    -- Addition and subtraction
    [ infixLeft "+" HiFunAdd,
      infixLeft "-" HiFunSub
    ],
    -- Equality and comparison
    [ infixNone "==" HiFunEquals,
      infixNone "/=" HiFunNotEquals,
      infixNone ">=" HiFunNotLessThan,
      infixNone "<=" HiFunNotGreaterThan,
      infixNone ">"  HiFunGreaterThan,
      infixNone "<"  HiFunLessThan
    ],
    -- Logical AND
    [ infixRight "&&" HiFunAnd
    ],
    -- Logical OR
    [ infixRight "||" HiFunOr
    ]
  ]
  where
    -- | Defines a division operator avoiding confusion with comments (e.g., '/' not followed by '=').
    divisionOperator :: String -> Operator Parser HiExpr
    divisionOperator _ = InfixL (createBinaryOperator HiFunDiv <$ try (char '/' <* notFollowedBy (char '=')))

    -- | Creates an infix operator with left associativity.
    infixLeft :: String -> HiFun -> Operator Parser HiExpr
    infixLeft name func = InfixL (createBinaryOperator func <$ symbolSequence name)

    -- | Creates an infix operator with right associativity.
    infixRight :: String -> HiFun -> Operator Parser HiExpr
    infixRight name func = InfixR (createBinaryOperator func <$ symbolSequence name)

    -- | Creates an infix operator with no associativity.
    infixNone :: String -> HiFun -> Operator Parser HiExpr
    infixNone name func = InfixN (createBinaryOperator func <$ symbolSequence name)

    -- | Constructs a binary operator function.
    createBinaryOperator :: HiFun -> HiExpr -> HiExpr -> HiExpr
    createBinaryOperator func leftOperand rightOperand =
      HiExprApply (HiExprValue $ HiValueFunction func) [leftOperand, rightOperand]

    -- | Parses a sequence of characters, treating them as a symbol.
    symbolSequence :: String -> Parser String
    symbolSequence = L.symbol space

-------------------- [ VALUE PARSING ] -------------------

-- | Parses different types of 'HiValue'.
parseValue :: Parser HiValue
parseValue =
  skipWhitespaces
    *> choice
      [ HiValueNumber   <$> parseRational,
        HiValueFunction <$> parseFunction,
        HiValueBool     <$> parseBoolean,
        HiValueNull     <$  parseNull,
        HiValueString   <$> parseString,
        HiValueBytes    <$> parseByteList,
        HiValueAction   <$> parseCurrentWorkingDirectory,
        HiValueAction   <$> parseCurrentTime
      ]

-- | Parses a rational number.
parseRational :: Parser Rational
parseRational =
  skipWhitespaces
    *> (toRational <$> L.signed skipWhitespaces L.scientific)
    <* skipWhitespaces

-- | Parses a function from its string representation.
parseFunction :: Parser HiFun
parseFunction =
  skipWhitespaces *>
    choice
      [ -- Numbers and arithmetic
        HiFunAdd            <$ string "add",
        HiFunDiv            <$ string "div",
        HiFunMul            <$ string "mul",
        HiFunSub            <$ string "sub",
        -- Booleans and comparison
        HiFunAnd            <$ string "and",
        HiFunOr             <$ string "or",
        HiFunLessThan       <$ string "less-than",
        HiFunGreaterThan    <$ string "greater-than",
        HiFunEquals         <$ string "equals",
        HiFunNotLessThan    <$ string "not-less-than",
        HiFunNotGreaterThan <$ string "not-greater-than",
        HiFunNotEquals      <$ string "not-equals",
        HiFunNot            <$ string "not",
        HiFunIf             <$ string "if",
        -- Strings and slices
        HiFunLength         <$ string "length",
        HiFunToUpper        <$ string "to-upper",
        HiFunToLower        <$ string "to-lower",
        HiFunReverse        <$ string "reverse",
        HiFunTrim           <$ string "trim",
        -- Lists and folds
        HiFunList           <$ string "list",
        HiFunRange          <$ string "range",
        HiFunFold           <$ string "fold",
        -- Bytes and serialisation
        HiFunPackBytes      <$ string "pack-bytes",
        HiFunUnpackBytes    <$ string "unpack-bytes",
        HiFunEncodeUtf8     <$ string "encode-utf8",
        HiFunDecodeUtf8     <$ string "decode-utf8",
        HiFunZip            <$ string "zip",
        HiFunUnzip          <$ string "unzip",
        HiFunSerialise      <$ string "serialise",
        HiFunDeserialise    <$ string "deserialise",
        -- File I/O
        HiFunRead           <$ string "read",
        HiFunWrite          <$ string "write",
        HiFunMkDir          <$ string "mkdir",
        HiFunChDir          <$ string "cd",
        -- Date and time
        HiFunParseTime      <$ string "parse-time",
        -- Random numbers
        HiFunRand           <$ string "rand",
        -- Short-circuit evaluation
        HiFunEcho           <$ string "echo",
        -- Dictionaries
        HiFunCount          <$ string "count",
        HiFunKeys           <$ string "keys",
        HiFunValues         <$ string "values",
        HiFunInvert         <$ string "invert"
      ]

-- | Parses a boolean value.
parseBoolean :: Parser Bool
parseBoolean =
  skipWhitespaces *>
    choice [
      True  <$ string "true",
      False <$ string "false"
    ]
  <* skipWhitespaces

-- | Parses a 'null' keyword.
parseNull :: Parser String
parseNull = skipWhitespaces *> string "null"

-- | Parses a string value.
parseString :: Parser DT.Text
parseString = DT.pack <$>
  (skipWhitespaces *>
    (char '\"' *> manyTill L.charLiteral (char '\"'))
  <* skipWhitespaces)

-- | Parses a list expression.
parseListExpression :: Parser HiExpr
parseListExpression = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> inSquareBrackets parseFunctionArguments

-- | Parses a byte list.
parseByteList :: Parser DB.ByteString
parseByteList = DB.pack <$> inHashBrackets (skipWhitespaces *> many parseByte <* skipWhitespaces)

-- | Parses a single byte.
parseByte :: Parser Word8
parseByte = L.lexeme space L.hexadecimal

-- | Parses the 'cwd' (current working directory) action.
parseCurrentWorkingDirectory :: Parser HiAction
parseCurrentWorkingDirectory = HiActionCwd <$ (skipWhitespaces *> string "cwd")

-- | Parses the 'now' (current time) action.
parseCurrentTime :: Parser HiAction
parseCurrentTime = HiActionNow <$ (skipWhitespaces *> string "now")

----------------- [ DICTIONARY PARSING ] -----------------

-- | Parses a dictionary expression.
parseDictionaryExpression :: Parser HiExpr
parseDictionaryExpression = HiExprDict <$> inFigureBrackets parseDictionaryPairs

-- | Parses a series of key-value pairs for a dictionary.
parseDictionaryPairs :: Parser [(HiExpr, HiExpr)]
parseDictionaryPairs = skipWhitespaces *> sepBy parseKeyValuePair (char ',')

-- | Parses a single key-value pair in a dictionary.
parseKeyValuePair :: Parser (HiExpr, HiExpr)
parseKeyValuePair = do
    keyExpr <- parseExpression
    void $ char ':' -- Consumes the colon separating the key and value.
    valueExpr <- parseExpression
    return (keyExpr, valueExpr)

------ [ HELPER FUNCTIONS FOR SPECIFIC STRUCTURES ] ------

inParentheses :: Parser a -> Parser a
inParentheses = inBrackets "(" ")"

inSquareBrackets :: Parser a -> Parser a
inSquareBrackets = inBrackets "[" "]"

inHashBrackets :: Parser a -> Parser a
inHashBrackets = inBrackets "[#" "#]"

inFigureBrackets :: Parser a -> Parser a
inFigureBrackets = inBrackets "{" "}"

------------------ [ UTILITY FUNCTIONS ] -----------------

inBrackets :: String -> String -> Parser a -> Parser a
inBrackets open close = between (skipWhitespaces *> string open) (string close <* skipWhitespaces)

skipWhitespaces :: Parser ()
skipWhitespaces = skipMany spaceChar
