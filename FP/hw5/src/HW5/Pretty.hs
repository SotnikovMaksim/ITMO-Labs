module HW5.Pretty
  ( prettyValue,
  )
where

import qualified Data.ByteString as DB hiding (map)
import Data.Char (intToDigit)
import Data.Foldable (toList)
import qualified Data.Map as DM
import Data.Ratio
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import Data.Time
import Data.Word (Word8)
import HW5.Base
import Prettyprinter hiding (prettyList)
import Prettyprinter.Render.Terminal

-- | Pretty-print a 'HiValue' to a 'Doc AnsiStyle'.
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
    
-- | Pretty-print a 'HiAction' to a 'Doc AnsiStyle'.
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction action = case action of
  HiActionRead path        -> pretty $ "read(\"" ++ path ++ "\")"
  HiActionWrite path bytes -> pretty $ "write(" ++ path ++ ", " ++ show (prettyBytes bytes) ++ ")"
  HiActionChDir path       -> pretty $ "cd(" ++ path ++ ")"
  HiActionMkDir path       -> pretty $ "mkdir(" ++ path ++ ")"
  HiActionRand x y         -> pretty $ "rand(" ++ show x ++ ", " ++ show y ++ ")"
  HiActionEcho text        -> pretty $ "echo(" ++ show text ++ ")"
  HiActionCwd              -> pretty "cwd"
  HiActionNow              -> pretty "now"
  
-- | Pretty-print a 'DS.Seq HiValue' as a list to a 'Doc AnsiStyle'.
prettyList :: DS.Seq HiValue -> Doc AnsiStyle
prettyList l
  | DS.null l = pretty "[ ]"
  | otherwise = encloseSep (pretty "[ ") (pretty " ]") (pretty ", ") (prettyValue <$> toList l)
  
-- | Pretty-print a 'DM.Map HiValue HiValue' as a dictionary to a 'Doc AnsiStyle'.
prettyDict :: DM.Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict
  | DM.null dict = pretty "{ }"
  | otherwise    = pretty "{ " <+> concatWith (surround (pretty ", ")) (map prettyDictPair (DM.toList dict)) <+> pretty " }"
  where
    prettyDictPair :: (HiValue, HiValue) -> Doc AnsiStyle
    prettyDictPair (key, value) = prettyValue key <> pretty ": " <> prettyValue value

-- | Pretty-print a 'Rational' number to a 'Doc AnsiStyle'.
prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber n
  | denominator n == 1 = pretty (numerator n)
  | otherwise = prettyFractionOrDecimal n

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

-- | Pretty-print a 'DT.Text' to a 'Doc AnsiStyle'.
prettyString :: DT.Text -> Doc AnsiStyle
prettyString str = pretty ("\"" <> DT.unpack str <> "\"")

-- | Pretty-print a 'DB.ByteString' to a 'Doc AnsiStyle'.
prettyBytes :: DB.ByteString -> Doc AnsiStyle
prettyBytes bytes
  | DB.null bytes = pretty "[# #]"
  | otherwise = pretty "[#" <+> concatWith (surround (pretty " ")) (map prettyByte (DB.unpack bytes)) <+> pretty "#]"

-- | Pretty-print a 'Word8' to a 'Doc AnsiStyle'.
prettyByte :: Word8 -> Doc AnsiStyle
prettyByte x = pretty (intToDigit $ div (fromIntegral x) 16) <> pretty (intToDigit $ mod (fromIntegral x) 16)

-- | Pretty-print a boolean type to a 'Doc AnsiStyle'.
prettyBool :: Bool -> Doc AnsiStyle
prettyBool bool
  | bool = pretty "true"
  | otherwise = pretty "false"

-- | Pretty-print a function to a 'Doc AnsiStyle'.
prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction func = pretty $
  case func of
    HiFunDiv             -> "div"
    HiFunMul             -> "mul"
    HiFunAdd             -> "add"
    HiFunSub             -> "sub"
    HiFunNot             -> "not"
    HiFunAnd             -> "and"
    HiFunOr              -> "or"
    HiFunLessThan        -> "less"
    HiFunGreaterThan     -> "greater"
    HiFunEquals          -> "equals"
    HiFunNotLessThan     -> "not-less-than"
    HiFunNotGreaterThan  -> "not-greater-than"
    HiFunNotEquals       -> "not-equals"
    HiFunIf              -> "if"
    HiFunLength          -> "length"
    HiFunToUpper         -> "to-upper"
    HiFunToLower         -> "to-lower"
    HiFunReverse         -> "reverse"
    HiFunTrim            -> "trim"
    HiFunList            -> "list"
    HiFunRange           -> "range"
    HiFunFold            -> "fold"
    HiFunPackBytes       -> "pack-bytes"
    HiFunUnpackBytes     -> "unpack-bytes"
    HiFunEncodeUtf8      -> "encode-utf8"
    HiFunDecodeUtf8      -> "decode-utf8"
    HiFunZip             -> "zip"
    HiFunUnzip           -> "unzip"
    HiFunSerialise       -> "serialise"
    HiFunDeserialise     -> "deserialise"
    HiFunRead            -> "read"
    HiFunWrite           -> "write"
    HiFunMkDir           -> "mkdir"
    HiFunChDir           -> "cd"
    HiFunParseTime       -> "parse-time"
    HiFunRand            -> "rand"
    HiFunEcho            -> "echo"
    HiFunCount           -> "count"
    HiFunKeys            -> "keys"
    HiFunValues          -> "values"
    HiFunInvert          -> "invert"
