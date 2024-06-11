module TestT3
  ( testTSize,
    testTDepth,
    testTMember,
    testTInsert,
    testTFromList,
  )
where

import HW1.T3
import Test.HUnit.Base (Test (TestList), (~?=))

-- Тесты для tsize
testTSize :: Test
testTSize =
  TestList
    [ tsize Leaf ~?= 0,
      tsize (Branch (Meta 1 1) Leaf 'a' Leaf) ~?= 1,
      tsize (Branch (Meta 3 2) (Branch (Meta 1 1) Leaf 'a' Leaf) 'b' (Branch (Meta 1 1) Leaf 'c' Leaf)) ~?= 3
    ]

-- Тесты для tdepth
testTDepth :: Test
testTDepth =
  TestList
    [ tdepth Leaf ~?= 0,
      tdepth (Branch (Meta 1 1) Leaf 'a' Leaf) ~?= 1,
      tdepth (Branch (Meta 3 2) (Branch (Meta 1 1) Leaf 'a' Leaf) 'b' (Branch (Meta 1 1) Leaf 'c' Leaf)) ~?= 2
    ]

-- Тесты для tmember
testTMember :: Test
testTMember =
  TestList
    [ tmember 'a' Leaf ~?= False,
      tmember 'a' (Branch (Meta 1 1) Leaf 'a' Leaf) ~?= True,
      tmember 'b' (Branch (Meta 3 2) (Branch (Meta 1 1) Leaf 'a' Leaf) 'b' (Branch (Meta 1 1) Leaf 'c' Leaf)) ~?= True,
      tmember 'x' (Branch (Meta 3 2) (Branch (Meta 1 1) Leaf 'a' Leaf) 'b' (Branch (Meta 1 1) Leaf 'c' Leaf)) ~?= False
    ]

-- Тесты для tinsert
testTInsert :: Test
testTInsert =
  TestList
    [ tinsert 'a' Leaf ~?= Branch (Meta 1 1) Leaf 'a' Leaf,
      tinsert 'b' (Branch (Meta 1 1) Leaf 'a' Leaf) ~?= Branch (Meta 2 2) Leaf 'a' (Branch (Meta 1 1) Leaf 'b' Leaf),
      tinsert 'c' (Branch (Meta 3 2) (Branch (Meta 1 1) Leaf 'a' Leaf) 'b' (Branch (Meta 1 1) Leaf 'c' Leaf))
        ~?= Branch
          (Meta 3 2)
          (Branch (Meta 1 1) Leaf 'a' Leaf)
          'b'
          (Branch (Meta 1 1) Leaf 'c' Leaf)
    ]

-- Тесты для tFromList
testTFromList :: Test
testTFromList =
  TestList
    [ tFromList "" ~?= Leaf,
      tFromList "abc"
        ~?= Branch
          (Meta 3 2)
          (Branch (Meta 1 1) Leaf 'a' Leaf)
          'b'
          (Branch (Meta 1 1) Leaf 'c' Leaf),
      tFromList "cba"
        ~?= Branch
          (Meta 3 2)
          (Branch (Meta 1 1) Leaf 'a' Leaf)
          'b'
          (Branch (Meta 1 1) Leaf 'c' Leaf)
    ]
