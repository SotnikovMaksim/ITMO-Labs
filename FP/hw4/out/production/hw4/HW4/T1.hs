module HW4.T1
  ( EvaluationError (..),
    ExceptState (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    eval,
  )
where

import qualified Control.Monad (ap)
import HW4.Types

-- | A monad transformer that adds error handling using 'Except' to stateful computations.
data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) } -- HLint suggests to use 'newtype' instead of 'data'

-- | Maps a function over the result of a stateful computation with error handling.
-- It transforms the result while preserving the state and possible errors.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState transform (ES runState) = ES $ \currentState ->
  case runState currentState of
    Error err -> Error err
    Success (result :# newState) ->
      let transformedResult = transform result
      in Success (transformedResult :# newState)

-- | Wraps a value into an error-handling stateful computation. Doesn't change the state.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState value = ES (\initialState -> Success (value :# initialState))

-- | Flattens a nested error-handling stateful computation.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState outerState = ES $ \currentState ->
  case runES outerState currentState of
    Error   err                       -> Error err
    Success (innerState :# nextState) -> runES innerState nextState

-- | Modifies the state within an error-handling stateful computation.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState modifyFunction = ES $ \currentState ->
  Success (() :# modifyFunction currentState)

-- | Throws an error within an error-handling stateful computation.
throwExceptState :: e -> ExceptState e s a
throwExceptState err = ES $ \_ -> Error err

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure    = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

-- | Represents different types of errors that can occur during evaluation.
data EvaluationError = DivideByZero
  deriving (Show, Eq)

-- | Evaluates an arithmetic expression within a stateful computation that logs operations and handles errors.
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val value)    = wrapExceptState value
eval (Op operation) = evalOp operation

-- | Evaluates binary arithmetic operations, logs the operation, and returns the result.
evalBinOp ::
  (Double -> Double -> Double) ->            -- ^ The binary operation to apply.
  (Double -> Double -> Prim Double) ->       -- ^ Constructor function for logging.
  Expr -> Expr ->                            -- ^ The two expressions to be evaluated.
  ExceptState EvaluationError [Prim Double] Double
evalBinOp binaryOp constructor leftExpr rightExpr = do
  leftValue  <- eval leftExpr
  rightValue <- eval rightExpr
  modifyExceptState (constructor leftValue rightValue :)
  return (binaryOp leftValue rightValue)

-- | Evaluates unary arithmetic operations, logs the operation, and returns the result.
evalUnaryOp ::
  (Double -> Double) ->               -- ^ The unary operation to apply.
  (Double -> Prim Double) ->          -- ^ Constructor function for logging.
  Expr ->                             -- ^ The expression to be evaluated.
  ExceptState EvaluationError [Prim Double] Double
evalUnaryOp unaryOp constructor expr = do
  value <- eval expr
  modifyExceptState (constructor value :)
  return (unaryOp value)

-- | Specifically handles division, including error handling for divide-by-zero scenarios.
evalDiv ::
  Expr -> Expr ->                      -- ^ The numerator and denominator expressions.
  ExceptState EvaluationError [Prim Double] Double
evalDiv numeratorExpr denominatorExpr = do
  numerator   <- eval numeratorExpr
  denominator <- eval denominatorExpr
  if denominator == 0
    then throwExceptState DivideByZero
    else do
      modifyExceptState (Div numerator denominator :)
      return (numerator / denominator)

-- | Delegates the evaluation of different arithmetic operations to their respective functions.
evalOp :: Prim Expr -> ExceptState EvaluationError [Prim Double] Double
evalOp (Add x y) = evalBinOp   (+)    Add x y
evalOp (Sub x y) = evalBinOp   (-)    Sub x y
evalOp (Mul x y) = evalBinOp   (*)    Mul x y
evalOp (Div x y) = evalDiv x y
evalOp (Abs x)   = evalUnaryOp abs    Abs x
evalOp (Sgn x)   = evalUnaryOp signum Sgn x
