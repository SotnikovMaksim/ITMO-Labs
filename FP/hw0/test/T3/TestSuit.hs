module TestSuit
  ( testS
  , testK
  , testI
  , testCompose
  , testContract
  , testPermute
  )
where

import HW0.T3
import Test.HUnit.Base (Test (TestList), (~:), (~?=))
import Data.List (sort)

-- Test for 's' combinator
testS :: Test
testS = "s" ~:
  TestList
    [ "s with addition and multiplication" ~: s (+) (*2) 3 ~?= (9 :: Integer)  -- (3 + (3 * 2))
    , "s with subtraction and division" ~: s (-) (`div` 2) 10 ~?= (5 :: Integer)  -- (10 - (10 `div` 2))
    , "s with list operations" ~: s (++) (take 3) [1..6] ~?= ([1..6] ++ [1,2,3] :: [Integer])
    ]

-- Test for 'k' combinator
testK :: Test
testK = "k" ~:
  TestList
    [ k 5 (10 :: Integer) ~?= (5 :: Integer)
    , k "hello" "world" ~?= "hello"
    ]

-- Test for 'i' combinator
testI :: Test
testI = "i" ~:
  TestList
    [ i 42 ~?= (42 :: Integer)
    , i "test" ~?= "test"
    ]

-- Test for 'compose' function
testCompose :: Test
testCompose = "compose" ~:
  TestList
    [ compose (*2) (+1) 3 ~?= (8 :: Integer)  -- (3 + 1) * 2
    , compose reverse sort "haskell" ~?= "sllkhea"
    ]

-- Test for 'contract' function
testContract :: Test
testContract = "contract" ~:
  TestList
    [ contract (*) 3 ~?= (9 :: Integer)  -- 3 * 3
    , contract (++) "test" ~?= "testtest"
    ]

-- Test for 'permute' function
testPermute :: Test
testPermute = "permute" ~:
  TestList
    [ permute (-) 5 3 ~?= (-2 :: Integer)  -- 3 - 5
    , permute (++) "world" "hello" ~?= "helloworld"
    ]




