module HW2.T4
  ( DotString (..),
    Fun (..),
    Inclusive (..),
    ListPlus (..),
  )
where

-- | Represents a non-empty list with a custom appending operation.
data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show, Eq)

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last a     <> ma = a :+ ma
  (a :+ ma') <> ma = a :+ (ma' <> ma)

-- | Represents either a value of one of two possible types or both values together.
data Inclusive a b = This a | That b | Both a b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (This a)   <> (This b)     = This (a <> b)
  (This a)   <> (That b)     = Both a b
  (That a)   <> (This b)     = Both b a
  (That a)   <> (That b)     = That (a <> b)
  (This a)   <> (Both b c)   = Both (a <> b) c
  (That a)   <> (Both b c)   = Both b (a <> c)
  (Both a b) <> (This c)     = Both (a <> c) b
  (Both a b) <> (That c)     = Both a (b <> c)
  (Both a b) <> (Both a' b') = Both (a <> a') (b <> b')

-- | Represents a string that, when concatenated, inserts a dot between two non-empty strings.
newtype DotString = DS String
  deriving (Show, Eq)

instance Semigroup DotString where
  left      <> (DS "")    = left
  (DS "")   <> right      = right
  (DS left) <> (DS right) = DS (left ++ "." ++ right)

instance Monoid DotString where
  mempty = DS ""

-- | Represents a function from type 'a' to 'a' with composition as the operation.
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (F f) <> (F g) = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
