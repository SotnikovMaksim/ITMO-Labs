module HW3.T4
  ( State (..),
    Prim (..),
    Expr (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    eval,
  )
where

import qualified Control.Monad (ap)
import HW3.T1

-- | The 'State' type represents a stateful computation that produces a value of type 'a'
-- and a new state of type 's'.
newtype State s a = S { runS :: s -> Annotated s a }

-- | Apply a function to the result of a stateful computation.
mapState :: (a -> b) -> State s a -> State s b
mapState transform (S runState) = S $ \currentState ->
  let (value :# newState) = runState currentState
      newValue            = transform value
   in newValue :# newState

-- | Wrap a value into a stateful computation that does not alter the state.
wrapState :: a -> State s a
wrapState value = S (value :#)

-- | Flatten a nested stateful computation.
joinState :: State s (State s a) -> State s a
joinState outerState = S $ \currentState ->
    let (innerState :# nextState)  = runS outerState currentState
        (finalValue :# finalState) = runS innerState nextState
    in finalValue :# finalState

-- | Modify the state within a stateful computation.
modifyState :: (s -> s) -> State s ()
modifyState transform = S $ \currentState ->
    let newState = transform currentState
    in () :# newState

-- | Define 'State' as a 'Functor'.
instance Functor (State s) where
  fmap = mapState

-- | Define 'State' as an 'Applicative'.
instance Applicative (State s) where
  pure = wrapState
  (<*>) = Control.Monad.ap -- Use 'ap' from 'Control.Monad' for the applicative style.

-- | Define 'State' as a 'Monad'.
instance Monad (State s) where
  m >>= f = joinState (fmap f m)

-- | The 'Prim' type represents primitive arithmetic operations on values of type 'a'.
data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show, Eq)

-- | The 'Expr' type represents arithmetic expressions.
data Expr = Val Double | Op (Prim Expr)
  deriving (Show, Eq)

-- | Make 'Expr' an instance of 'Num' to support standard arithmetic operations.
instance Num Expr where
  x + y         = Op  (Add x y)
  x - y         = Op  (Sub x y)
  x * y         = Op  (Mul x y)
  abs x         = Op  (Abs x)
  signum x      = Op  (Sgn x)
  fromInteger x = Val (fromInteger x)

-- | Make 'Expr' an instance of 'Fractional' to support fractional operations.
instance Fractional Expr where
  x / y          = Op  (Div x y)
  fromRational x = Val (fromRational x)

-- | Evaluate an 'Expr' into a stateful computation that logs the primitive operations performed.
eval :: Expr -> State [Prim Double] Double
eval (Val x)   = pure x
eval (Op prim) = evalOp prim

-- | Evaluate a binary operation, logging the operation and returning the result.
evalBinOp ::
  (Double -> Double -> Double) ->
  (Double -> Double -> Prim Double) ->
  Expr ->
  Expr ->
  State [Prim Double] Double
evalBinOp op constructor x y = do
  a <- eval x
  b <- eval y
  modifyState (constructor a b :)
  return (op a b)

-- | Evaluate a unary operation, logging the operation and returning the result.
evalUnaryOp ::
  (Double -> Double) ->
  (Double -> Prim Double) ->
  Expr ->
  State [Prim Double] Double
evalUnaryOp op constructor x = do
  a <- eval x
  modifyState (constructor a :)
  return (op a)

-- | Helper function to evaluate an operation, delegating to the correct evaluation function.
evalOp :: Prim Expr -> State [Prim Double] Double
evalOp (Add x y) = evalBinOp (+)      Add x y
evalOp (Sub x y) = evalBinOp (-)      Sub x y
evalOp (Mul x y) = evalBinOp (*)      Mul x y
evalOp (Div x y) = evalBinOp (/)      Div x y
evalOp (Abs x)   = evalUnaryOp abs    Abs x
evalOp (Sgn x)   = evalUnaryOp signum Sgn x