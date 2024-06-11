module HW5.Pretty
  ( prettyValue,
  )
where

import Data.Ratio
import Data.Word (Word8)
import qualified Data.ByteString as DB hiding (map)
import qualified Data.Sequence as DS
import qualified Data.Map as DM
import qualified Data.Text as DT
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import HW5.Base
import Prettyprinter.Render.Terminal
import Prettyprinter hiding (prettyList)
import Data.Foldable (toList)
import Data.Char (intToDigit)
import Data.Time

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue value =
  case value of
    (HiValueNumber number) -> prettyNumber number
    (HiValueFunction func) -> prettyFunction func
    (HiValueBool bool)     -> prettyBool bool
    HiValueNull            -> pretty "null"
    (HiValueString str)    -> prettyString str
    (HiValueList l)        -> prettyList l
    (HiValueBytes bytes)   -> prettyBytes bytes
    (HiValueAction action) -> prettyAction action
    (HiValueTime time)     -> prettyTime time
    (HiValueDict dict)     -> prettyDict dict
  
prettyList :: DS.Seq HiValue -> Doc AnsiStyle
prettyList l
  | DS.null l = pretty "[ ]"
  | otherwise = encloseSep (pretty "[ ") (pretty " ]") (pretty ", ") (prettyValue <$> toList l)
  
prettyDict :: DM.Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict
  | DM.null dict = pretty "{ }"
  | otherwise    = pretty "{ " <+> concatWith (surround (pretty ", ")) (map prettyDictPair (DM.toList dict)) <+> pretty " }"
  where
    prettyDictPair :: (HiValue, HiValue) -> Doc AnsiStyle
    prettyDictPair (key, value) = prettyValue key <> pretty ": " <> prettyValue value

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber n
  | denominator n == 1 = pretty (numerator n)
  | otherwise = prettyFractionOrDecimal n

-- TODO: cleanup separating 'pretty' call
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction action = case action of
  (HiActionRead path)        -> pretty "read(\"" <> pretty path <> pretty "\")"
  (HiActionWrite path bytes) -> pretty "write(" <> pretty path <> pretty ", " <> prettyBytes bytes <> pretty ")"
  (HiActionChDir path)       -> pretty "cd(" <> pretty path <> pretty ")"
  (HiActionMkDir path)       -> pretty "mkdir(" <> pretty path <> pretty ")"
  (HiActionRand x y)         -> pretty "rand(" <> pretty x <> pretty ", " <> pretty y <> pretty ")"
  (HiActionEcho text)        -> pretty "echo(" <> pretty text <> pretty ")"
  HiActionCwd                -> pretty "cwd"
  HiActionNow                -> pretty "now"

-- Pretty prints a UTCTime type
prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime time = pretty ("parse-time(\"" <> show time <> "\")")

-- Pretty prints a fraction or a decimal
prettyFractionOrDecimal :: Rational -> Doc AnsiStyle
prettyFractionOrDecimal n =
  case fromRationalRepetendUnlimited n of
    (s, Nothing) -> pretty (formatScientific Fixed Nothing s) -- Decimal representation
    _ -> prettyFraction (numerator n, denominator n)

-- Pretty prints a fraction
prettyFraction :: (Integer, Integer) -> Doc AnsiStyle
prettyFraction (num, denom) =
  let (whole, remainder) = quotRem num denom
   in if whole /= 0
        then pretty whole <+> plusOrMinus remainder <+> pretty (abs remainder) <> slash <> pretty denom
        else pretty remainder <> slash <> pretty denom

-- Determines whether to print '+' or '-'
plusOrMinus :: Integer -> Doc AnsiStyle
plusOrMinus r = pretty (if r < 0 then '-' else '+')

prettyString :: DT.Text -> Doc AnsiStyle
prettyString str = pretty ("\"" <> DT.unpack str <> "\"")

prettyBytes :: DB.ByteString -> Doc AnsiStyle
prettyBytes bytes
  | DB.null bytes = pretty "[# #]"
  | otherwise = pretty "[#" <+> concatWith (surround (pretty " ")) (map prettyByte (DB.unpack bytes)) <+> pretty "#]"

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte x = pretty (intToDigit $ div (fromIntegral x) 16) <> pretty (intToDigit $ mod (fromIntegral x) 16)

prettyBool :: Bool -> Doc AnsiStyle
prettyBool bool
  | bool = pretty "true"
  | otherwise = pretty "false"

prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction func =
  pretty $
    case func of
      -- Task 1: Numbers and arithmetic
      HiFunDiv -> "div"
      HiFunMul -> "mul"
      HiFunAdd -> "add"
      HiFunSub -> "sub"
      -- Task 2: Booleans and comparison
      HiFunNot            -> "not"
      HiFunAnd            -> "and"
      HiFunOr             -> "or"
      HiFunLessThan       -> "less"
      HiFunGreaterThan    -> "greater"
      HiFunEquals         -> "equals"
      HiFunNotLessThan    -> "not-less-than"
      HiFunNotGreaterThan -> "not-greater-than"
      HiFunNotEquals      -> "not-equals"
      HiFunIf             -> "if"
      -- Task 4: Strings and slices
      HiFunLength  -> "length"
      HiFunToUpper -> "to-upper"
      HiFunToLower -> "to-lower"
      HiFunReverse -> "reverse"
      HiFunTrim    -> "trim"
      --  Task 5: Lists and folds
      HiFunList  -> "list"
      HiFunRange -> "range"
      HiFunFold  -> "fold"
      -- Task 6: Bytes and serialisation
      HiFunPackBytes   -> "pack-bytes"
      HiFunUnpackBytes -> "unpack-bytes"
      HiFunEncodeUtf8  -> "encode-utf8"
      HiFunDecodeUtf8  -> "decode-utf8"
      HiFunZip         -> "zip"
      HiFunUnzip       -> "unzip"
      HiFunSerialise   -> "serialise"
      HiFunDeserialise -> "deserialise"
      -- Task 7: File I/O
      HiFunRead  -> "read"
      HiFunWrite -> "write"
      HiFunMkDir -> "mkdir"
      HiFunChDir -> "cd"
      -- Task 8: Date and time
      HiFunParseTime -> "parse-time"
      -- Task 9: Random numbers
      HiFunRand -> "rand"
      -- Task 10: Short-circuit evaluation
      HiFunEcho -> "echo"
      -- Task 11: Dictionaries
      HiFunCount  -> "count"
      HiFunKeys   -> "keys"
      HiFunValues -> "values"
      HiFunInvert -> "invert"