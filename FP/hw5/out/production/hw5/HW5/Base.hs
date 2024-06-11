{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HW5.Base
  ( HiError (..),
    HiExpr (..),
    HiFun (..),
    HiValue (..),
    HiAction (..),
    HiMonad (..)
  )
where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Represents all the supported functions in the language.
data HiFun
  = HiFunAdd            -- ^ Addition function.
  | HiFunSub            -- ^ Subtraction function.
  | HiFunMul            -- ^ Multiplication function.
  | HiFunDiv            -- ^ Division function.
  | HiFunNot            -- ^ Logical NOT function.
  | HiFunAnd            -- ^ Logical AND function.
  | HiFunOr             -- ^ Logical OR function.
  | HiFunLessThan       -- ^ Less than comparison.
  | HiFunGreaterThan    -- ^ Greater than comparison.
  | HiFunEquals         -- ^ Equality comparison.
  | HiFunNotLessThan    -- ^ Not less than comparison.
  | HiFunNotGreaterThan -- ^ Not greater than comparison.
  | HiFunNotEquals      -- ^ Not equal comparison.
  | HiFunIf             -- ^ Conditional function.
  | HiFunLength         -- ^ String length function.
  | HiFunToUpper        -- ^ Convert to upper case.
  | HiFunToLower        -- ^ Convert to lower case.
  | HiFunReverse        -- ^ Reverse a string or list.
  | HiFunTrim           -- ^ Trim whitespace from a string.
  | HiFunList           -- ^ Create a list.
  | HiFunRange          -- ^ Create a range of numbers.
  | HiFunFold           -- ^ Fold (reduce) function.
  | HiFunPackBytes      -- ^ Pack data into bytes.
  | HiFunUnpackBytes    -- ^ Unpack bytes into data.
  | HiFunEncodeUtf8     -- ^ Encode a string to UTF-8 bytes.
  | HiFunDecodeUtf8     -- ^ Decode UTF-8 bytes to a string.
  | HiFunZip            -- ^ Compress data.
  | HiFunUnzip          -- ^ Decompress data.
  | HiFunSerialise      -- ^ Serialise data.
  | HiFunDeserialise    -- ^ Deserialise data.
  | HiFunRead           -- ^ Read from a file.
  | HiFunWrite          -- ^ Write to a file.
  | HiFunMkDir          -- ^ Create a directory.
  | HiFunChDir          -- ^ Change current directory.
  | HiFunParseTime      -- ^ Parse a time string.
  | HiFunRand           -- ^ Generate random numbers.
  | HiFunEcho           -- ^ Echo a string.
  | HiFunCount          -- ^ Count occurrences.
  | HiFunKeys           -- ^ Retrieve keys from a dictionary.
  | HiFunValues         -- ^ Retrieve values from a dictionary.
  | HiFunInvert         -- ^ Invert a dictionary.
  deriving (Eq, Ord, Show, Generic)
  deriving (Serialise)

-- | Represents the different types of values in the language.
data HiValue
  = HiValueNumber Rational            -- ^ Represents a rational number.
  | HiValueFunction HiFun             -- ^ Represents a function.
  | HiValueBool Bool                  -- ^ Represents a boolean value.
  | HiValueNull                       -- ^ Represents a null value.
  | HiValueString Text                -- ^ Represents a text string.
  | HiValueList (Seq HiValue)         -- ^ Represents a list of values.
  | HiValueBytes ByteString           -- ^ Represents byte data.
  | HiValueAction HiAction            -- ^ Represents an action (functions ended on '!' in REPL).
  | HiValueTime UTCTime               -- ^ Represents a UTC time value.
  | HiValueDict (Map HiValue HiValue) -- ^ Represents a dictionary.
  deriving (Eq, Ord, Show, Generic)
  deriving (Serialise)


-- | Represents expressions in the language.
data HiExpr
  = HiExprValue HiValue           -- ^ Represents a literal value.
  | HiExprApply HiExpr [HiExpr]   -- ^ Represents a function application.
  | HiExprRun HiExpr              -- ^ Represents a command to run an action.
  | HiExprDict [(HiExpr, HiExpr)] -- ^ Represents a dictionary expression.
  deriving (Show)


-- | Represents different types of errors that can occur.
data HiError
  = HiErrorInvalidArgument   -- ^ Error for invalid argument.
  | HiErrorInvalidFunction   -- ^ Error for invalid function.
  | HiErrorArityMismatch     -- ^ Error for arity mismatch.
  | HiErrorDivideByZero      -- ^ Error for division by zero (e.g. in HiFunDiv function).
  deriving (Show)
  
-- | Represents actions that can be performed.
data HiAction
  = HiActionRead FilePath              -- ^ Read from a file.
  | HiActionWrite FilePath ByteString  -- ^ Write to a file.
  | HiActionMkDir FilePath             -- ^ Create a directory.
  | HiActionChDir FilePath             -- ^ Change current directory.
  | HiActionCwd                        -- ^ Get current working directory.
  | HiActionNow                        -- ^ Get current time.
  | HiActionRand Int Int               -- ^ Generate a random number.
  | HiActionEcho Text                  -- ^ Echo a text string.
  deriving (Eq, Ord, Show, Generic, Serialise)
  
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
