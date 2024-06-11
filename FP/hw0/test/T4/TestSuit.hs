module TestSuit
  ( testRepeat
  , testMap
  , testFib
  , testFac
  )
where

import HW0.T4
import Test.HUnit.Base (Test (TestList), (~:), (~?=))

-- Test for 'repeat''
testRepeat :: Test
testRepeat = "repeat'" ~:
  TestList
    [ take 5 (repeat' (3 :: Integer)) ~?= [3, 3, 3, 3, 3]
    , take 4 (repeat' "hi") ~?= ["hi", "hi", "hi", "hi"]
    ]

-- Test for 'map''
testMap :: Test
testMap = "map'" ~:
  TestList
    [ map' (*(2 :: Integer)) [1, 2, 3]   ~?= [2, 4, 6]
    , map' show ([1, 2, 3] :: [Integer]) ~?= ["1", "2", "3"]
    ]

-- Test for 'fib'
testFib :: Test
testFib = "fib" ~:
  TestList
    [ fib 0 ~?= 0
    , fib 1 ~?= 1
    , fib 5 ~?= 5
    , fib 10 ~?= 55
    ]

-- Test for 'fac'
testFac :: Test
testFac = "fac" ~:
  TestList
    [ fac 0 ~?= 1
    , fac 1 ~?= 1
    , fac 5 ~?= 120
    , fac 10 ~?= 3628800
    ]
