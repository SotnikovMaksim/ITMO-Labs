{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

-- | An isomorphism between two types 'a' and 'b'.
--   Consists of a pair of functions: one to convert 'a' to 'b', and another to convert 'b' back to 'a'.
data a <-> b = Iso (a -> b) (b -> a)

-- | Flips an isomorphism.
--   Given an isomorphism from 'a' to 'b', it provides an isomorphism from 'b' to 'a'.
flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

-- | Runs the forward direction of an isomorphism.
--   Given an isomorphism and a value of type 'a', it produces a value of type 'b'.
runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

-- | Distributes an 'Either' over a pair.
--   Transforms an 'Either a (b, c)' into a pair '(Either a b, Either a c)'.
distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

-- | An isomorphism between a nested pair and a rearranged pair.
--   Converts between '(a, (b, c))' and '((a, b), c)'.
assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso left right
  where
    left  (a, (b, c)) = ((a, b), c)
    right ((a, b), c) = (a, (b, c))

-- | An isomorphism between nested 'Either' types.
--   Converts between 'Either a (Either b c)' and 'Either (Either a b) c'.
assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso left right
  where
    left  (Left a)          = Left (Left a)
    left  (Right (Left b))  = Left (Right b)
    left  (Right (Right c)) = Right c 
    right (Left (Left a))   = Left a
    right (Left (Right b))  = Right (Left b)
    right (Right c)         = Right (Right c)
    