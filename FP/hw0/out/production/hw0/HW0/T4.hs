module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function (fix)

-- | Generates an infinite list of repeated values.
repeat' :: a -> [a]
repeat' x = fix (x : )

-- | Custom map function using recursion.
map' :: (a -> b) -> [a] -> [b]
map' func = fix (\rec xs ->
  case xs of
    []       -> []
    (y : ys) -> func y : rec ys)

-- | Computes the nth Fibonacci number.
fib :: Natural -> Natural
fib = fix (\rec prev cur n ->
  if n == 0 then prev
    else rec cur (prev + cur) (n - 1)) 0 1 -- Start values are 0 and 1

-- | Computes the factorial of a 'Natural' number.
fac :: Natural -> Natural
fac = fix (\rec n -> 
  if n <= 1 then 1
    else n * rec (n - 1))
