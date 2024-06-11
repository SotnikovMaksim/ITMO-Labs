module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

-- | Represents natural numbers using Church encoding.
type Nat a = (a -> a) -> a -> a

-- | The Church numeral 0.
nz :: Nat a
nz _ a = a

-- | The successor function for Church numerals.
-- Given a Church numeral 'n', 'ns n' represents 'n + 1'.
ns :: Nat a -> Nat a
ns a f b = f (a f b)

-- | Adds two Church numerals.
-- Given two Church numerals 'a' and 'b', it creates a new numeral that represents their sum.
nplus :: Nat a -> Nat a -> Nat a
nplus a b f = a f . b f

-- | Multiplies two Church numerals.
-- Given two Church numerals 'a' and 'b', it returns a numeral representing their product.
nmult :: Nat a -> Nat a -> Nat a
nmult a b = a . b

-- | Converts a standard 'Natural' number to a Church numeral.
-- This function takes a 'Natural' number 'n' and recursively constructs the corresponding Church numeral.
nFromNatural :: Natural -> Nat a
nFromNatural n
  | n == 0    = nz
  | otherwise = ns (nFromNatural (n - 1))

-- | Converts a Church numeral to a standard numeric type.
-- This function takes a Church numeral 'n' and applies the increment function (+1) starting from 0,
nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0
