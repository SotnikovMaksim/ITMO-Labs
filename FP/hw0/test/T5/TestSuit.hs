module TestSuit
  ( testNz
  , testNs
  , testNplus
  , testNmult
  , testNFromNatural
  )
where

import HW0.T5
import Test.HUnit.Base (Test (TestList), (~:), (~?=))

-- Test for 'nz'
testNz :: Test
testNz = "nz" ~:
  TestList
    [ nToNum (nz :: Nat Integer) ~?= 0
    , nToNum (nz :: Nat Double) ~?= 0.0   -- why not
    ]

-- Test for 'ns'
testNs :: Test
testNs = "ns" ~:
  TestList
    [ nToNum (ns nz :: Nat Integer) ~?= 1
    , nToNum (ns (ns nz) :: Nat Integer) ~?= 2
    , nToNum (ns (ns (ns nz)) :: Nat Integer) ~?= 3
    , nToNum (ns (ns (ns (ns nz))) :: Nat Integer) ~?= 4
    ]

-- Test for 'nplus'
testNplus :: Test
testNplus = "nplus" ~:
  TestList
    [ nToNum (nplus (ns nz) (ns nz) :: Nat Integer) ~?= 2
    , nToNum (nplus (ns (ns nz)) (ns nz) :: Nat Integer) ~?= 3
    , nToNum (nplus nz (ns (ns nz)) :: Nat Integer) ~?= 2
    , nToNum (nplus (ns (ns (ns nz))) (ns (ns nz)) :: Nat Integer) ~?= 5
    ]

-- Test for 'nmult'
testNmult :: Test
testNmult = "nmult" ~:
  TestList
    [ nToNum (nmult (ns nz) (ns nz) :: Nat Integer) ~?= 1
    , nToNum (nmult (ns (ns nz)) (ns (ns nz)) :: Nat Integer) ~?= 4
    , nToNum (nmult nz (ns (ns nz)) :: Nat Integer) ~?= 0
    , nToNum (nmult (ns (ns (ns nz))) (ns nz) :: Nat Integer) ~?= 3
    ]

-- Test for 'nFromNatural'
testNFromNatural :: Test
testNFromNatural = "nFromNatural" ~:
  TestList
    [ nToNum (nFromNatural 0 :: Nat Integer) ~?= 0
    , nToNum (nFromNatural 5 :: Nat Integer) ~?= 5
    , nToNum (nFromNatural 3 :: Nat Integer) ~?= 3
    , nToNum (nFromNatural 1 :: Nat Integer) ~?= 1
    , nToNum (nFromNatural 10 :: Nat Integer) ~?= 10
    ]


