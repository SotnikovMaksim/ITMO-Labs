module HW2.T1
  ( Tree (..),
    tfoldr
  )
where

-- | A binary tree with an integer counter in each Branch.
data Tree a
  = Leaf                                   -- ^ Represents an empty tree node.
  | Branch !Int (Tree a) a (Tree a)        -- ^ Branch with integer meta inf, left subtree, value, and right subtree.
  deriving (Show)

-- | Fold for the Tree structure, traversing right-to-left.
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ accumulator Leaf = accumulator
tfoldr mapper accumulator (Branch _ leftSubtree root rightSubtree) =
  tfoldr 
    mapper
    (mapper root (tfoldr mapper accumulator rightSubtree)) 
    leftSubtree
