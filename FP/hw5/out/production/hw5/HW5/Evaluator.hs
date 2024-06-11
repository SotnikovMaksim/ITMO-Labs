module HW5.Evaluator
  ( eval,
  )
where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
    throwE,
  )
import Data.Foldable (toList)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.ByteString as DB
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Map as DM
import Data.Maybe
import HW5.Base
import Codec.Compression.Zlib
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Codec.Serialise (deserialiseOrFail, serialise)
import Text.Read (readMaybe)
import Data.Time (addUTCTime, UTCTime(..), diffUTCTime)

data Arity
  = Invalid
  | None
  | Unary
  | Binary
  | Ternary
  deriving (Show)

------------------- [ CORE EVALUATION ] ------------------

eval :: (HiMonad m) => HiExpr -> m (Either HiError HiValue)
eval expression = runExceptT $ evaluateExpression expression

-- TODO: lazy evaluation: don't evaluate HiExpr here
-- | Evaluates various types of Hi expressions.
evaluateExpression :: (HiMonad m) => HiExpr -> ExceptT HiError m HiValue
evaluateExpression expr =
  case expr of
    HiExprValue value -> return value
    HiExprApply expression arguments -> do
      evaluatedExpr <- evaluateExpression expression
      case evaluatedExpr of
        HiValueFunction func | isLazyEvaluation func -> evaluateLazily func arguments
        _ -> evaluateFunction evaluatedExpr (determineArity evaluatedExpr) arguments
    HiExprDict pairList  -> mapM evaluateKeyValuePair pairList >>= createDictionary
    HiExprRun actionExpr -> evaluateExpression actionExpr >>= performAction

-- | Dispatches function evaluation based on arity.
evaluateFunction :: (HiMonad m) => HiValue -> Arity -> [HiExpr] -> ExceptT HiError m HiValue
evaluateFunction value arity args = case (value, arity) of
  (_, None) -> evaluateWithNoArity value args
  (HiValueFunction func, Unary)   -> evaluateUnary func args
  (HiValueFunction func, Binary)  -> evaluateBinary func args
  (HiValueFunction func, Ternary) -> evaluateTernary func args
  _ -> throwE HiErrorInvalidFunction

-- | Executes an action and returns its result.
performAction :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
performAction action = case action of
  HiValueAction act -> lift $ runAction act
  _ -> throwE HiErrorInvalidArgument

------------------- [ LAZY EVALUATION ] ------------------

-- | Determines if a function can be lazy evaluated.
isLazyEvaluation :: HiFun -> Bool
isLazyEvaluation func = func `elem` [HiFunAnd, HiFunOr]

-- | Lazily evaluates binary boolean functions (`And`, `Or`).
evaluateLazily :: (HiMonad m) => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evaluateLazily HiFunAnd = evaluateLazilyWithPredicate isFalsy
evaluateLazily HiFunOr  = evaluateLazilyWithPredicate isTruthy
evaluateLazily _        = const $ throwE HiErrorInvalidFunction

-- | Performs lazy evaluation of boolean expressions based on a given predicate.
evaluateLazilyWithPredicate :: (HiMonad m) => (HiValue -> Bool) -> [HiExpr] -> ExceptT HiError m HiValue
evaluateLazilyWithPredicate predicate [firstExpr, secondExpr] = do
  evaluatedFirst <- evaluateExpression firstExpr
  if predicate evaluatedFirst
    then return evaluatedFirst
    else evaluateExpression secondExpr
evaluateLazilyWithPredicate _ _ = throwE HiErrorInvalidArgument

-- | Checks if a 'HiValue' is falsy.
isFalsy :: HiValue -> Bool
isFalsy HiValueNull = True
isFalsy (HiValueBool False) = True
isFalsy _ = False

-- | Checks if a 'HiValue' is truthy.
isTruthy :: HiValue -> Bool
isTruthy = not . isFalsy

---------------- [ NO ARITY OPERATIONS ] -----------------

-- | Evaluates operations that do not have a predefined arity.
evaluateWithNoArity :: (HiMonad m) => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evaluateWithNoArity value args = do
  evaluatedArgs <- mapM evaluateExpression args
  case value of
    str@(HiValueString _)     -> performSlicing str evaluatedArgs
    list@(HiValueList _)      -> performSlicing list evaluatedArgs
    HiValueFunction HiFunList -> return $ HiValueList $ DS.fromList evaluatedArgs
    bytes@(HiValueBytes _)    -> performSlicing bytes evaluatedArgs
    dict@(HiValueDict _)      -> getValueByKey dict evaluatedArgs
    _                         -> throwE HiErrorInvalidArgument

-- | Performs slicing operations on strings, lists, and byte arrays.
performSlicing :: (HiMonad m) => HiValue -> [HiValue] -> ExceptT HiError m HiValue
performSlicing value indexes = case value of
  HiValueString str  -> sliceString str indexes
  HiValueList list   -> sliceList list indexes
  HiValueBytes bytes -> sliceBytes bytes indexes
  _                  -> throwE HiErrorInvalidArgument

-- | Slice a 'DT.Text' based on the given indices or index range.
sliceString :: (HiMonad m) => DT.Text -> [HiValue] -> ExceptT HiError m HiValue
sliceString str [i] = do
  let len = fromIntegral $ DT.length str
  safeIndexResult <- safeIndex i len
  case safeIndexResult of
    HiValueNumber startIndex -> do
      let start = fromIntegral (numerator startIndex)
          end = start + 1
      return $ HiValueString (DT.take (end - start) $ DT.drop start str)
    _ -> return HiValueNull
sliceString str [l, r] = do
  let len = fromIntegral $ DT.length str
  (startIndex, endIndex) <- safeIndexes l r len
  let start = fromIntegral startIndex
      end = fromIntegral endIndex
  return $ HiValueString (DT.take (end - start) $ DT.drop start str)
sliceString _ _ = throwE HiErrorInvalidArgument

-- | Slice a 'DS.Seq HiValue' based on the given indices or index range.
sliceList :: (HiMonad m) => DS.Seq HiValue -> [HiValue] -> ExceptT HiError m HiValue
sliceList list [i] = do
  let len = DS.length list
  safeIndexResult <- safeIndex i (fromIntegral len)
  case safeIndexResult of
    HiValueNumber startIndex ->
      return $ fromMaybe HiValueNull (DS.lookup (fromIntegral $ numerator startIndex) list)
    _ -> return HiValueNull
sliceList list [l, r] = do
  let len = fromIntegral $ DS.length list
  (startIndex, endIndex) <- safeIndexes l r len
  let start = fromIntegral startIndex
      end = fromIntegral endIndex
  return $ HiValueList (DS.take (end - start) $ DS.drop start list)
sliceList _ _ = throwE HiErrorInvalidArgument

-- | Slice a 'DB.ByteString' based on the given indices or index range.
sliceBytes :: (HiMonad m) => DB.ByteString -> [HiValue] -> ExceptT HiError m HiValue
sliceBytes bytes [i] = do
  safeIndexResult <- safeIndex i (fromIntegral $ DB.length bytes)
  case safeIndexResult of
    HiValueNumber startIndex ->
      let idx = fromIntegral $ numerator startIndex
          byte = DB.index bytes idx
      in return $ HiValueNumber (fromIntegral byte)
    _ -> return HiValueNull
sliceBytes bytes [l, r] = do
  let len = fromIntegral $ DB.length bytes
  (startIndex, endIndex) <- safeIndexes l r len
  let start = fromIntegral startIndex
      end = fromIntegral endIndex
  return $ HiValueBytes (DB.take (end - start) $ DB.drop start bytes)
sliceBytes _ _ = throwE HiErrorInvalidArgument

-- | Safely index a list-like structure using a 'HiValue' index.
safeIndex :: (HiMonad m) => HiValue -> Integer -> ExceptT HiError m HiValue
safeIndex (HiValueNumber i) len = do
  if denominator i /= 1
    then throwE HiErrorInvalidArgument
    else let num = numerator i
         in return $ if num < 0 || num >= len then HiValueNull else HiValueNumber (toRational num)
safeIndex _ _ = throwE HiErrorInvalidArgument

-- | Safely compute the start and end indices for slicing a list-like structure
-- using 'HiValue' indices or index range.
safeIndexes :: (HiMonad m) => HiValue -> HiValue -> Integer -> ExceptT HiError m (Integer, Integer)
safeIndexes HiValueNull r@(HiValueNumber _) len = safeIndexes (HiValueNumber 0) r len
safeIndexes l@(HiValueNumber _) HiValueNull len = safeIndexes l (HiValueNumber (fromIntegral len)) len
safeIndexes (HiValueNumber l) (HiValueNumber r) len = do
  if denominator l /= 1 || denominator r /= 1
    then throwE HiErrorInvalidArgument
    else let numL = numerator l
             numR = numerator r
             adjustedL = if numL < 0 then numL + len else numL
             adjustedR = if numR < 0 then numR + len else numR
         in if adjustedL > adjustedR
            then throwE HiErrorInvalidArgument
            else return (adjustedL, adjustedR)
safeIndexes _ _ _ = throwE HiErrorInvalidArgument

------------------ [ UNARY OPERATIONS ] ------------------

-- | Evaluates unary operations.
evaluateUnary :: (HiMonad m) => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evaluateUnary function [expr] = evaluateExpression expr >>= performUnaryOperation function
evaluateUnary _ _ = throwE HiErrorInvalidArgument

-- | Performs the specified unary operation on the given value.
performUnaryOperation :: (HiMonad m) => HiFun -> HiValue -> ExceptT HiError m HiValue
performUnaryOperation function value =
  case function of
    -- Booleans and comparison
    HiFunNot           -> applyNot value
    -- String operations
    HiFunLength        -> applyLength value
    HiFunToUpper       -> applyToUpper value
    HiFunToLower       -> applyToLower value
    HiFunTrim          -> applyTrim value
    -- List operations
    HiFunReverse       -> applyReverse value
    -- Byte and serialization operations
    HiFunPackBytes     -> packBytes value
    HiFunUnpackBytes   -> unpackBytes value
    HiFunEncodeUtf8    -> encodeUtf8String value
    HiFunDecodeUtf8    -> decodeUtf8Bytes value
    HiFunZip           -> zipBytes value
    HiFunUnzip         -> unzipBytes value
    HiFunSerialise     -> serialiseValue value
    HiFunDeserialise   -> deserialiseBytes value
    -- File I/O
    HiFunRead          -> readFileAction value
    HiFunChDir         -> changeDirectoryAction value
    HiFunMkDir         -> makeDirectoryAction value
    -- Date and time
    HiFunParseTime     -> parseTimeFromString value
    -- Short-circuit evaluation
    HiFunEcho          -> echoString value
    -- Dictionary operations
    HiFunKeys          -> extractDictionaryKeys value
    HiFunValues        -> extractDictionaryValues value
    HiFunInvert        -> invertDictionary value
    HiFunCount         -> countElements value
    _                  -> throwE HiErrorInvalidArgument

------------------ Booleans and comparison -----------------

-- | Applies the 'not' operation to a boolean value.
applyNot :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
applyNot value = case value of
  HiValueBool bool -> return $ HiValueBool $ not bool
  _ -> throwE HiErrorInvalidArgument

--------------------- String operations --------------------

-- | Returns the length of a string or list.
applyLength :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
applyLength value = case value of
  HiValueString str -> return $ HiValueNumber $ toRational $ DT.length str
  HiValueList list  -> return $ HiValueNumber $ toRational $ DS.length list
  _ -> throwE HiErrorInvalidArgument

-- | Converts a string to uppercase.
applyToUpper :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
applyToUpper value = case value of
  HiValueString s -> return $ HiValueString $ DT.toUpper s
  _ -> throwE HiErrorInvalidArgument

-- | Converts a string to lowercase.
applyToLower :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
applyToLower value = case value of
  HiValueString s -> return $ HiValueString $ DT.toLower s
  _ -> throwE HiErrorInvalidArgument

-- | Trims whitespace from a string.
applyTrim :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
applyTrim value = case value of
  HiValueString s -> return $ HiValueString $ DT.strip s
  _ -> throwE HiErrorInvalidArgument

--------------------- List operations ----------------------

-- | Reverses a string or list.
applyReverse :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
applyReverse value = case value of
  HiValueString s -> return $ HiValueString $ DT.reverse s
  HiValueList list -> return $ HiValueList $ DS.reverse list
  _ -> throwE HiErrorInvalidArgument

------------ Byte and serialization operations -------------

-- | Packs a list of integers into bytes.
packBytes :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
packBytes value = case value of
  HiValueList list -> HiValueBytes . DB.pack <$> mapM integralFromHiValue (toList list)
  _ -> throwE HiErrorInvalidArgument

-- | Unpacks bytes into a list of integers.
unpackBytes :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
unpackBytes value = case value of
  HiValueBytes bytes -> return (HiValueList . DS.fromList $ map (HiValueNumber . fromIntegral) (DB.unpack bytes))
  _ -> throwE HiErrorInvalidArgument

-- | Encodes a string to UTF-8 bytes.
encodeUtf8String :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
encodeUtf8String value = case value of
  HiValueString str -> return $ HiValueBytes (encodeUtf8 str)
  _ -> throwE HiErrorInvalidArgument

-- | Decodes UTF-8 bytes to a string.
decodeUtf8Bytes :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
decodeUtf8Bytes value = case value of
  HiValueBytes bytes -> return $ either (const HiValueNull) HiValueString (decodeUtf8' bytes)
  _ -> throwE HiErrorInvalidArgument

-- | Compresses bytes using zlib.
zipBytes :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
zipBytes value =
  case value of
    HiValueBytes bytes ->
      return $ HiValueBytes (toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } $ fromStrict bytes)
    _ -> throwE HiErrorInvalidArgument

-- | Decompresses bytes using zlib.
unzipBytes :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
unzipBytes value = case value of
  HiValueBytes bytes -> return $ HiValueBytes (toStrict $ decompressWith defaultDecompressParams $ fromStrict bytes)
  _ -> throwE HiErrorInvalidArgument

-- | Serializes a HiValue to bytes.
serialiseValue :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
serialiseValue value = return $ HiValueBytes (toStrict $ serialise value)

-- | Deserializes bytes to a HiValue.
deserialiseBytes :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
deserialiseBytes value =
  case value of
    HiValueBytes bytes -> deserialise bytes
    _ -> throwE HiErrorInvalidArgument

------------------------- File I/O -------------------------

-- | Prepares a file read action.
readFileAction :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
readFileAction value =
  case value of
    HiValueString filePath -> return $ HiValueAction $ HiActionRead $ DT.unpack filePath
    _ -> throwE HiErrorInvalidArgument

-- | Prepares a change directory action.
changeDirectoryAction :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
changeDirectoryAction value =
  case value of
    HiValueString dirName -> return $ HiValueAction $ HiActionChDir $ DT.unpack dirName
    _ -> throwE HiErrorInvalidArgument

-- | Prepares a make directory action.
makeDirectoryAction :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
makeDirectoryAction value =
  case value of
    HiValueString dirName -> return $ HiValueAction $ HiActionMkDir $ DT.unpack dirName
    _ -> throwE HiErrorInvalidArgument

----------------------- Date and time ----------------------

-- | Parses a time string to a HiValueTime.
parseTimeFromString :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
parseTimeFromString value =
  case value of
    HiValueString timeStr -> parseTime timeStr
    _ -> throwE HiErrorInvalidArgument

------------------ Numbers and arithmetic ------------------

-- | Echoes a string as an action.
echoString :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
echoString value = case value of
  HiValueString str -> return $ HiValueAction $ HiActionEcho str
  _ -> throwE HiErrorInvalidArgument

------------------ [ BINARY OPERATIONS ] -----------------

-- | Evaluates binary operations.
evaluateBinary :: (HiMonad m) => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evaluateBinary function [expr1, expr2] = do
  evaluatedExpr1 <- evaluateExpression expr1
  evaluatedExpr2 <- evaluateExpression expr2
  performBinaryOperation function evaluatedExpr1 evaluatedExpr2
evaluateBinary _ _ = throwE HiErrorArityMismatch

-- | Implements binary operations based on the provided function and values.
performBinaryOperation :: (HiMonad m) => HiFun -> HiValue -> HiValue -> ExceptT HiError m HiValue
performBinaryOperation function x y = case function of
  -- Numbers and arithmetic (including strings concatenation etc.)
  HiFunAdd               -> performAddition x y
  HiFunSub               -> performSubtraction x y
  HiFunMul               -> performMultiplication x y
  HiFunDiv               -> performDivision x y
  -- Booleans and comparison
  HiFunLessThan          -> applyLessThan x y
  HiFunGreaterThan       -> applyGreaterThan x y
  HiFunEquals            -> applyEquals x y
  HiFunNotEquals         -> applyNotEquals x y
  HiFunNotLessThan       -> applyNotLessThan x y
  HiFunNotGreaterThan    -> applyNotGreaterThan x y
  -- List operations
  HiFunFold              -> foldListFunction x y
  HiFunRange             -> createRange x y
  -- File I/O
  HiFunWrite             -> writeInFile x y
  -- Random numbers
  HiFunRand              -> randomInRange x y
  _                      -> throwE HiErrorInvalidArgument

------------------ Numbers and arithmetic ------------------

-- Implementation of HiFunAdd function. If another was provided, error will be thrown
performAddition :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
performAddition x y =
  case (x, y) of
    (HiValueNumber n1, HiValueNumber n2)    -> return $ HiValueNumber (n1 + n2)
    (HiValueString s1, HiValueString s2)    -> return $ HiValueString (s1 <> s2)
    (HiValueList lst1, HiValueList lst2)    -> return $ HiValueList (lst1 <> lst2)
    (HiValueBytes bs1, HiValueBytes bs2)    -> return $ HiValueBytes (bs1 <> bs2)
    (HiValueTime time, HiValueNumber delta) -> addTime time delta
    _ -> throwE HiErrorInvalidArgument

-- Implementation of HiFunSub function. If another was provided, error will be thrown
performSubtraction :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
performSubtraction x y =
  case (x, y) of
    (HiValueNumber n1, HiValueNumber n2)   -> return $ HiValueNumber (n1 - n2)
    (HiValueTime time1, HiValueTime time2) -> diffTime time1 time2
    _ -> throwE HiErrorInvalidArgument

-- Implementation of HiFunMul function. If another was provided, error will be thrown
performMultiplication :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
performMultiplication x y =
  case (x, y) of
    (HiValueNumber n1, HiValueNumber n2)         -> return $ HiValueNumber (n1 * n2)
    (str@(HiValueString _), k@(HiValueNumber _)) -> multiplier str k
    (list@(HiValueList _),  k@(HiValueNumber _)) -> multiplier list k
    (list@(HiValueBytes _), k@(HiValueNumber _)) -> multiplier list k
    _ -> throwE HiErrorInvalidArgument

-- Implementation of HiFunDiv function. If another was provided, error will be thrown
performDivision :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
performDivision x y =
  case (x, y) of
    (HiValueNumber _,  HiValueNumber 0)      -> throwE HiErrorDivideByZero
    (HiValueNumber n1, HiValueNumber n2)     -> return $ HiValueNumber (n1 / n2)
    (HiValueString str1, HiValueString str2) -> return $ HiValueString (str1 <> DT.pack "/" <> str2)
    _ -> throwE HiErrorInvalidArgument

----------------- Booleans and comparison -----------------

-- Implementation of HiFunLessThan function. If another was provided, error will be thrown
applyLessThan :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
applyLessThan x y = comparisonFunctionsTemplate (x, y) True False (x < y)

-- Implementation of HiFunGreaterThan function. If another was provided, error will be thrown
applyGreaterThan :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
applyGreaterThan x y = comparisonFunctionsTemplate (x, y) False True (x > y)

-- Implementation of HiFunEquals function. If another was provided, error will be thrown
applyEquals :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
applyEquals x y = comparisonFunctionsTemplate (x, y) False False (x == y)

-- Implementation of HiFunNotEquals function. If another was provided, error will be thrown
applyNotEquals :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
applyNotEquals x y = comparisonFunctionsTemplate (x, y) True True (x /= y)

-- Implementation of HiFunAdd function. If another was provided, error will be thrown
applyNotLessThan :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
applyNotLessThan x y = comparisonFunctionsTemplate (x, y) False True (x >= y)

-- Implementation of HiFunNotGreaterThan function. If another was provided, error will be thrown
applyNotGreaterThan :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
applyNotGreaterThan x y = comparisonFunctionsTemplate (x, y) True False (x <= y)

-- Template of implementation for boolean binary boolean comparisons since we can't compare HiValueBool with HiValueNumber  
comparisonFunctionsTemplate :: (HiMonad m) => (HiValue, HiValue) -> Bool -> Bool -> Bool -> ExceptT HiError m HiValue
comparisonFunctionsTemplate pair fc sc res = return $
  case pair of
    (HiValueBool _, HiValueNumber _) -> HiValueBool fc
    (HiValueNumber _, HiValueBool _) -> HiValueBool sc
    _                                -> HiValueBool res

--------------------- List operations ---------------------

-- | Fold a function over the elements of a list.
foldListFunction :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
foldListFunction x y =
  case (x, y) of
    (mapper, HiValueList list) -> foldList mapper (toList list)
    _ -> throwE HiErrorInvalidArgument

-- | Create a list of numbers in a given range.
createRange :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
createRange x y = case (x, y) of
  (HiValueNumber start, HiValueNumber end) -> createNumberList start end
  _ -> throwE HiErrorInvalidArgument

------------------------ File I/O ------------------------

-- | Write bytes to a file with the given name.
writeInFile :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
writeInFile x y =
  case (x, y) of
    (HiValueString fileName, HiValueBytes bytes) ->
      return $ HiValueAction $ HiActionWrite (DT.unpack fileName) bytes
    (HiValueString fileName, HiValueString str) ->
      return $ HiValueAction $ HiActionWrite (DT.unpack fileName) (encodeUtf8 str)
    _ -> throwE HiErrorInvalidArgument

--------------------- Random numbers ---------------------

-- | Generate a random number within the specified range.
randomInRange :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
randomInRange x y =
  case (x, y) of
    (HiValueNumber start, HiValueNumber end) -> handleRand start end
    _ -> throwE HiErrorInvalidArgument

-- | Multiply a string, list, or bytes by a numeric factor.
multiplier :: (HiMonad m) => HiValue -> HiValue -> ExceptT HiError m HiValue
multiplier (HiValueString str) num = do
  k <- validateMultiplier num
  return . HiValueString $ stimes k str
multiplier (HiValueList list) num = do
  k <- validateMultiplier num
  return . HiValueList $ stimes k list
multiplier (HiValueBytes bytes) num = do
  k <- validateMultiplier num
  return . HiValueBytes $ stimes k bytes
multiplier _ _ = throwE HiErrorInvalidFunction

-- | Validate and extract an integer multiplier from a 'HiValue'.
validateMultiplier :: (HiMonad m) => HiValue -> ExceptT HiError m Integer
validateMultiplier (HiValueNumber k)
  | k < 0 = throwE HiErrorInvalidArgument
  | denominator k /= 1 = throwE HiErrorInvalidArgument
  | otherwise = return $ numerator k
validateMultiplier _ = throwE HiErrorInvalidArgument

----------------- [ TERNARY OPERATIONS ] -----------------

-- | Evaluate a ternary (if-then-else) expression.
evaluateTernary :: (HiMonad m) => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evaluateTernary HiFunIf [x, y, z] = do
  evalResult <- evaluateExpression x
  case evalResult of
    (HiValueBool True)  -> evaluateExpression y
    (HiValueBool False) -> evaluateExpression z
    _                   -> throwE HiErrorInvalidArgument
evaluateTernary _ _ = throwE HiErrorArityMismatch

------------------- [ LIST OPERATIONS ] ------------------

-- | Folds a list using a specified function.
foldList :: (HiMonad m) => HiValue -> [HiValue] -> ExceptT HiError m HiValue
foldList func list = do
  case determineArity func of
    None   -> foldListHelper func list
    Binary -> foldListHelper func list
    _      -> throwE HiErrorInvalidArgument

-- | Helper function for folding a list with a specified function.
foldListHelper :: (HiMonad m) => HiValue -> [HiValue] -> ExceptT HiError m HiValue
foldListHelper _ [] = return HiValueNull
foldListHelper function (firstValue : restValues) = foldM applyFunction firstValue restValues
  where
    applyFunction accumulator currentValue = do
      evaluatedResult <- evaluateExpression $ HiExprApply (HiExprValue function)
                                              [HiExprValue accumulator, HiExprValue currentValue]
      case evaluatedResult of
        HiValueFunction _ -> throwE HiErrorInvalidFunction
        _                 -> return evaluatedResult

createNumberList :: (HiMonad m) => Rational -> Rational -> ExceptT HiError m HiValue
createNumberList start end =
  return (HiValueList . DS.fromList $ HiValueNumber <$> enumFromTo start end)

---------------- [ DICTIONARY OPERATIONS ] ---------------

-- | Creates a HiValue dictionary from a list of key-value pairs.
createDictionary :: (HiMonad m) => [(HiValue, HiValue)] -> ExceptT HiError m HiValue
createDictionary = return . HiValueDict . DM.fromList

-- | Evaluates a pair of expressions to a pair of HiValues.
evaluateKeyValuePair :: (HiMonad m) => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
evaluateKeyValuePair (keyExpr, valueExpr) = do
  evaluatedKey   <- evaluateExpression keyExpr
  evaluatedValue <- evaluateExpression valueExpr
  return (evaluatedKey, evaluatedValue)

-- | Retrieves a value from a dictionary by its key.
getValueByKey :: (HiMonad m) => HiValue -> [HiValue] -> ExceptT HiError m HiValue
getValueByKey (HiValueDict dict) [key] = return $ fromMaybe HiValueNull (DM.lookup key dict)
getValueByKey _ _ = throwE HiErrorInvalidArgument

-- | Extracts the keys from a dictionary.
extractDictionaryKeys :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
extractDictionaryKeys (HiValueDict dict) = return $ HiValueList $ DS.fromList $ DM.keys dict
extractDictionaryKeys _ = throwE HiErrorInvalidArgument

-- | Extracts the values from a dictionary.
extractDictionaryValues :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
extractDictionaryValues (HiValueDict dict) = return $ HiValueList $ DS.fromList $ DM.elems dict
extractDictionaryValues _ = throwE HiErrorInvalidArgument

-- | Inverts a dictionary by swapping its keys and values.
invertDictionary :: (HiMonad m) => HiValue -> ExceptT HiError m HiValue
invertDictionary (HiValueDict dict) =
  return . HiValueDict $ DM.map (HiValueList . DS.fromList) $ DM.fromListWith (++) flippedPairs
  where flippedPairs = [(v, [k]) | (k, v) <- DM.toList dict]
invertDictionary _ = throwE HiErrorInvalidArgument

-- | Counts the occurrences of each element in a string, list, or byte array.
countElements :: HiMonad m => HiValue -> ExceptT HiError m HiValue
countElements value =
  case value of
      HiValueString str  ->
        return (HiValueDict $ DM.mapKeys (HiValueString . DT.singleton) (DM.map HiValueNumber (count $ DT.unpack str)))
      HiValueList lst    ->
        return $ HiValueDict $ DM.map HiValueNumber (count $ toList lst)
      HiValueBytes bytes ->
        return $ HiValueDict $ DM.mapKeys (HiValueNumber . toRational . toInteger) (DM.map HiValueNumber (count $ DB.unpack bytes))
      _                  -> throwE HiErrorInvalidArgument
    where
      count :: Ord k => [k] -> DM.Map k Rational
      count s = DM.fromListWith (+) [(c, 1) | c <- s]

--------------- [ BYTE/STRING OPERATIONS ] ---------------

-- | Deserializes a ByteString into a HiValue, throwing an error for invalid input.
deserialise :: (HiMonad m) => DB.ByteString -> ExceptT HiError m HiValue
deserialise bytes =
  case deserialiseOrFail (fromStrict bytes) of
    Right value -> return value
    Left _  -> throwE HiErrorInvalidArgument

-- | Converts a HiValueNumber to a specified numeric type, ensuring it's a valid integral within 0-255.
integralFromHiValue :: (HiMonad m, Num a) => HiValue -> ExceptT HiError m a
integralFromHiValue (HiValueNumber r) = do
  let denom = denominator r
      num = numerator r
  if denom == 1 && num >= 0 && num <= 255
    then return $ fromIntegral num
    else throwE HiErrorInvalidArgument
integralFromHiValue _ = throwE HiErrorInvalidArgument

------------------- [ TIME OPERATIONS ] ------------------

-- | Parses a text representation of a date and time into a HiValueTime.
parseTime :: (HiMonad m) => DT.Text -> ExceptT HiError m HiValue
parseTime timeStr =
  case readMaybe (DT.unpack timeStr) :: Maybe UTCTime of
    Just time -> return $ HiValueTime time
    Nothing   -> return HiValueNull

-- | Adds a time delta to a UTCTime, returning the resulting time as HiValueTime.
addTime :: (HiMonad m) => UTCTime -> Rational -> ExceptT HiError m HiValue
addTime time delta = return (HiValueTime $ addUTCTime (fromRational delta) time)

-- | Calculates the difference between two UTCTime values, returning the result as HiValueNumber.
diffTime :: (HiMonad m) => UTCTime -> UTCTime -> ExceptT HiError m HiValue
diffTime time1 time2 = return (HiValueNumber . toRational $ diffUTCTime time1 time2)

-------------- [ RANDOM NUMBER GENERATION ] --------------

-- | Generates a random number action within the specified range, if valid.
handleRand :: (HiMonad m) => Rational -> Rational -> ExceptT HiError m HiValue
handleRand x y =
  case (rationalToInt x, rationalToInt y) of
    (Right intX, Right intY) -> return (HiValueAction $ HiActionRand intX intY)
    _                        -> throwE HiErrorInvalidArgument

------------------ [ UTILITY FUNCTIONS ] -----------------

-- | Safely converts a Rational to an Int, returning an error for non-integral values.
rationalToInt :: Rational -> Either HiError Int
rationalToInt rational =
  if denominator rational == 1
     then Right (fromIntegral (numerator rational))
     else Left HiErrorInvalidArgument

-- | Determines the arity of a given HiValue (especially HiValueFunction).
determineArity :: HiValue -> Arity
determineArity (HiValueFunction func) =
  case func of
    -- Numbers and arithmetic
    HiFunAdd             -> Binary
    HiFunSub             -> Binary
    HiFunMul             -> Binary
    HiFunDiv             -> Binary
    -- Booleans and comparison
    HiFunNot             -> Unary
    HiFunAnd             -> Binary
    HiFunOr              -> Binary
    HiFunLessThan        -> Binary
    HiFunGreaterThan     -> Binary
    HiFunEquals          -> Binary
    HiFunNotLessThan     -> Binary
    HiFunNotGreaterThan  -> Binary
    HiFunNotEquals       -> Binary
    HiFunIf              -> Ternary
    -- Strings and slices
    HiFunLength          -> Unary
    HiFunToUpper         -> Unary
    HiFunToLower         -> Unary
    HiFunReverse         -> Unary
    HiFunTrim            -> Unary
    -- Lists and folds
    HiFunList            -> None
    HiFunRange           -> Binary
    HiFunFold            -> Binary
    -- Bytes and serialisation
    HiFunPackBytes       -> Unary
    HiFunUnpackBytes     -> Unary
    HiFunEncodeUtf8      -> Unary
    HiFunDecodeUtf8      -> Unary
    HiFunZip             -> Unary
    HiFunUnzip           -> Unary
    HiFunSerialise       -> Unary
    HiFunDeserialise     -> Unary
    -- File I/O
    HiFunRead            -> Unary
    HiFunMkDir           -> Unary
    HiFunChDir           -> Unary
    HiFunWrite           -> Binary
    -- Date and time
    HiFunParseTime       -> Unary
    -- Random numbers
    HiFunRand            -> Binary
    -- Short-circuit evaluation
    HiFunEcho            -> Unary
    -- Dictionaries
    HiFunCount           -> Unary
    HiFunKeys            -> Unary
    HiFunValues          -> Unary
    HiFunInvert          -> Unary
determineArity (HiValueString _) = None
determineArity (HiValueList _)   = None
determineArity (HiValueBytes _)  = None
determineArity (HiValueDict _)   = None
determineArity _                 = Invalid
