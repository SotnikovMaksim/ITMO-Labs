-- | This module contains the types from hw3 that are also
-- needed for hw4.
{-# LANGUAGE FlexibleInstances #-}

module HW4.Types
  ( Annotated (..)
  , Except (..)
  , Expr (..)
  , Prim (..)
  , State (..)
  ) where

data Except e a = Error e | Success a
  deriving (Show, Eq)

data Annotated e a = a :# e
  deriving Show

data State s a = S { runS :: s -> Annotated s a }

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show
  
instance Eq Expr where
    (Val x) == (Val y) = x == y
    (Op x) == (Op y)   = x == y
    _ == _             = False
    
instance Eq a => Eq (Prim a) where
    (Add a1 b1) == (Add a2 b2) = a1 == a2 && b1 == b2
    (Sub a1 b1) == (Sub a2 b2) = a1 == a2 && b1 == b2
    (Mul a1 b1) == (Mul a2 b2) = a1 == a2 && b1 == b2
    (Div a1 b1) == (Div a2 b2) = a1 == a2 && b1 == b2
    (Abs a)     == (Abs b)     = a == b
    (Sgn a)     == (Sgn b)     = a == b
    _           == _           = False
 
--instance Eq (Prim Double) where
--    (Add a1 b1) == (Add a2 b2) = a1 == a2 && b1 == b2
--    (Sub a1 b1) == (Sub a2 b2) = a1 == a2 && b1 == b2
--    (Mul a1 b1) == (Mul a2 b2) = a1 == a2 && b1 == b2
--    (Div a1 b1) == (Div a2 b2) = a1 == a2 && b1 == b2
--    (Abs a)     == (Abs b)     = a == b
--    (Sgn a)     == (Sgn b)     = a == b
--    _           == _           = False

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)