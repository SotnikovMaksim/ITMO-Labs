module TestSuit
  ( testTfoldr
  )
where

import HW2.T1
import Test.HUnit.Base (Test (TestList), (~=?), (~:))

-- Testing 'tfoldr' function
testTfoldr :: Test
testTfoldr = TestList
  [ "tfoldr on Leaf" ~:
      (0 :: Int) ~=? tfoldr (+) 0 Leaf

  , "tfoldr on simple tree" ~:
      (6 :: Int) ~=? tfoldr (+) 0 (Branch 1 (Branch 2 Leaf 2 Leaf) 1 (Branch 2 Leaf 3 Leaf))
  ]