module HW1.T3
  ( Meta (..),
    Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Meta = Meta
  { size :: Int,
    depth :: Int
  }
  deriving (Show, Eq)

data Tree a
  = Leaf
  | Branch Meta (Tree a) a (Tree a)
  deriving (Show, Eq)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize a =
  case a of
    Leaf                -> 0
    (Branch meta _ _ _) -> size meta

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth a =
  case a of
    Leaf                -> 0
    (Branch meta _ _ _) -> depth meta

-- | Check if the element is in the tree, O(log n)
tmember :: (Ord a) => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ leftSubtree root rightSubtree)
  | a == root = True
  | a < root  = tmember a leftSubtree
  | otherwise = tmember a rightSubtree

-- | Insert an element into the tree, O(log n)
tinsert :: (Ord a) => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a tree@(Branch _ leftSubtree root rightSubtree)
  | a == root = tree
  | a < root  = balance (mkBranch (tinsert a leftSubtree) root rightSubtree)
  | otherwise = balance (mkBranch leftSubtree root (tinsert a rightSubtree))

-- | Build a tree from a list, O(n log n)
tFromList :: (Ord a) => [a] -> Tree a
tFromList = foldr tinsert Leaf

-- ================== Additional methods for AVL tree ==================
-- I respect clean code so I decided to give to methods exhaustive names.
-- Probably it seems strange and inappropriate in Haskell (if so let me know), but
-- I think it is much more readable than kinda 'tDepth'.

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch leftSubtree root rightSubtree =
  Branch
    ( Meta
        (1 + tsize leftSubtree + tsize rightSubtree)         -- recalculate size
        (1 + max (tdepth leftSubtree) (tdepth rightSubtree)) -- recalculate depth
    )
    leftSubtree
    root
    rightSubtree

-- Set new left subtree for provided
getLeftSubtreeFrom, getRightSubtreeFrom :: Tree a -> Tree a
getLeftSubtreeFrom Leaf = Leaf
getLeftSubtreeFrom (Branch _ left _ _) = left

getRightSubtreeFrom Leaf = Leaf
getRightSubtreeFrom (Branch _ _ _ right) = right

-- Set new left or right subtree for provided
setLeftSubtreeFor, setRightSubtreeFor :: Tree a -> Tree a -> Tree a
setLeftSubtreeFor Leaf _ = undefined
setLeftSubtreeFor (Branch meta _ root rightSubtree) tree =
  Branch meta tree root rightSubtree
  
setRightSubtreeFor Leaf _ = undefined
setRightSubtreeFor (Branch meta leftSubtree root _) tree =
  Branch meta leftSubtree root tree

-- Returns root of provided tree
getRootOf :: Tree a -> a
getRootOf Leaf                = undefined
getRootOf (Branch _ _ root _) = root

-- Splay tree rotations
rotateLeft, rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight (Branch _ leftSubtree root rightSubtree) =
  mkBranch
    (getLeftSubtreeFrom leftSubtree)       -- leftSubtree
    (getRootOf leftSubtree)                -- root
    ( mkBranch                             -- rightSubtree
        (getRightSubtreeFrom leftSubtree)  
        root
        rightSubtree
    )
    
rotateLeft Leaf = Leaf
rotateLeft (Branch _ leftSubtree root rightSubtree) =
  mkBranch
    ( mkBranch                             -- leftSubtree
        leftSubtree
        root
        (getLeftSubtreeFrom rightSubtree)
    )
    (getRootOf rightSubtree)               -- root
    (getRightSubtreeFrom rightSubtree)     -- rightSubtree

balanceValue :: Tree a -> Int
balanceValue Leaf = 0
balanceValue (Branch _ leftSubtree _ rightSubtree) =
  tdepth rightSubtree - tdepth leftSubtree

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance tree@(Branch _ leftSubtree _ rightSubtree) =
  case balanceValue tree of
    -2
      | balanceValue leftSubtree > 0 -> rotateRight (setLeftSubtreeFor tree (rotateLeft leftSubtree))
      | otherwise                    -> rotateRight tree
    2
      | balanceValue rightSubtree < 0 -> rotateLeft (setRightSubtreeFor tree (rotateRight rightSubtree))
      | otherwise                     -> rotateLeft tree
    _ -> tree
