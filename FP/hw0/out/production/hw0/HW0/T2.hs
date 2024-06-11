module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

-- | Represents the logical negation of a type.
type Not a = a -> Void

-- | Takes an element of type 'a' and returns its double negation.
doubleNeg :: a -> Not (Not a)
doubleNeg = flip id

-- | Reduces a triple negation to a single negation.
-- This function takes a triple negation of a type 'a' 
-- (i.e., a function that, given a double negation of 'a', leads to a contradiction)
-- and converts it into a single negation of 'a'. 
-- It leverages the 'doubleNeg' function to achieve this reduction.
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg = (. doubleNeg)
