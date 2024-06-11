module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where

-- | The 'Option' type encapsulates an optional value. A value of type 'Option' either
-- contains a value of type 'a' ('Some'), or it is empty ('None').
data Option a = None | Some a
  deriving (Eq, Show)

-- | Apply a function to the contents of an 'Option', if it is 'Some', otherwise
-- return 'None'.
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None     = None
mapOption f (Some a) = Some (f a)

-- | The 'Pair' type represents a pair of values of the same type.
data Pair a = P a a
  deriving (Eq, Show)

-- | Apply a function to both elements of a 'Pair'.
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

-- | The 'Quad' type represents a quadruple of values of the same type.
data Quad a = Q a a a a
  deriving (Eq, Show)

-- | Apply a function to all four elements of a 'Quad'.
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

-- | The 'Annotated' type represents a value of type 'a' annotated with a value of type 'e'.
data Annotated e a = a :# e
  deriving (Eq, Show)

infix 0 :#

-- | Apply a function to the value of an 'Annotated', leaving the annotation unchanged.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

-- | The 'Except' type represents a computation that may fail. It either contains
-- a successful result of type 'a' ('Success') or an error of type 'e' ('Error').
data Except e a = Error e | Success a
  deriving (Eq, Show)

-- | Apply a function to the result of an 'Except', if it is 'Success', otherwise
-- return the 'Error' unchanged.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success (f a)

-- | The 'Prioritised' type represents a value of type 'a' with an associated priority.
data Prioritised a = Low a | Medium a | High a
  deriving (Eq, Show)

-- | Apply a function to the value of a 'Prioritised', preserving its priority.
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low    (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a)   = High   (f a)

-- | The 'Stream' type represents an infinite stream of values of type 'a'.
data Stream a = a :> Stream a
  deriving (Eq, Show)

infixr 5 :>

-- | Apply a function to each element of a 'Stream'.
mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (current :> rest) = f current :> mapStream f rest

-- | The 'List' type represents a list of values of type 'a'.
data List a = Nil | a :. List a
  deriving (Eq, Show)

infixr 5 :.

-- | Apply a function to each element of a 'List'.
mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil               = Nil
mapList f (current :. rest) = f current :. mapList f rest

-- | The 'Fun' type represents a function from type 'i' to type 'a'.
data Fun i a = F (i -> a) -- HLint suggests 'newtype Fun i a = F (i -> a)' instead

-- | Transform the output of a 'Fun' using a given function.
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F (f . g)

-- | The 'Tree' type represents a binary tree with nodes of type 'a'.
data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

-- | Apply a function to each element of a 'Tree'.
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf                     = Leaf
mapTree f (Branch left root right) = Branch (mapTree f left) (f root) (mapTree f right)
