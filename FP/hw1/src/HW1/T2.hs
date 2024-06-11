-- TODO: refactor code - align statements

module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    nFromNatural,
    nToNum,
    ncmp,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Data.Maybe (fromJust)
import Numeric.Natural

data N = Z | S N deriving (Show, Eq)

-- addition
nplus :: N -> N -> N
nplus Z     = id -- First argument is neutral so we return id :: N -> N
nplus (S n) = nplus n . S -- First of, applying S to second argument, then applying nplus to n and (S .)

-- multiplication
nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult n (S m) = nplus n (nmult n m)

-- subtraction (Nothing if result is negative)
nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

-- comparison
ncmp :: N -> N -> Ordering
ncmp a b =
  case nsub a b of
    Nothing -> LT
    Just Z  -> EQ
    _       -> GT

nFromNatural :: Natural -> N
nFromNatural x =
  case x of
    0 -> Z
    _ -> S (nFromNatural (x - 1))

nToNum :: (Num a) => N -> a
nToNum x =
  case x of
    Z     -> 0
    (S n) -> nToNum n + 1

-- parity checking
nEven, nOdd :: N -> Bool
nEven x =
  case x of
    Z     -> True
    (S n) -> nOdd n
    
nOdd x =
  case x of
    Z     -> False
    (S n) -> nEven n

-- integer division
ndiv :: N -> N -> N
ndiv a b =
  case nsub a b of
    Nothing -> Z
    Just x  -> S (ndiv x b)

-- modulo operation
nmod :: N -> N -> N
nmod a b =
  let divRes = ndiv a b
   in fromJust $     -- 'fromJust' will throw an error if nsub return Nothing
      nsub a $
      nmult divRes b
