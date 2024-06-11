module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

-- | 's' ('substitution') combinator: Applies function 'f' to 'x' and the result of applying 'g' to 'x'.
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

-- | 'k' ('kestrel') combinator: Returns the first argument, ignoring the second.
k :: a -> b -> a
k x _ = x

-- | 'i' ('identity') combinator: Identity function, defined using 's' and 'k'.
i :: a -> a
i = s k k

-- | Composes two functions. Equivalent to (f . g) in standard Haskell.
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

-- | Contracts a function of two identical arguments into a single argument.
contract :: (a -> a -> b) -> (a -> b)
contract = s s (s k)

-- | Permutes the arguments of a binary function.
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k (s (k s) k)) s) (k k)
