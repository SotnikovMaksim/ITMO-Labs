{-# LANGUAGE TypeOperators #-}

module TestSuit
  ( testFlipIso
  , testRunIso
  , testDistrib
  , testAssocPair
  , testAssocEither
  )
where

import Test.HUnit.Base (Test (TestList), (~:), (~?=))
import HW0.T1

-- Test cases for flipIso
testFlipIso :: Test
testFlipIso = "flipIso" ~:
  TestList
    [ testIso (flipIso $ Iso (+1) (\x -> x - 1)) 5 (4 :: Integer)
    , testIso (flipIso $ Iso (*2) (`div` 2)) 10 (5 :: Integer)
    ]

-- Test cases for runIso
testRunIso :: Test
testRunIso = "runIso" ~:
  TestList
    [ runIso (Iso (+1) (\x -> x - 1)) 5 ~?= (6 :: Integer)
    , runIso (Iso (*2) (`div` 2)) 5 ~?= (10 :: Integer)
    ]

-- Test cases for distrib
testDistrib :: Test
testDistrib = "distrib" ~:
  TestList
      [ distrib (Left 5 :: Either Int (Int, String)) ~?= (Left 5, Left 5)
      , distrib (Right (7, "hello") :: Either Int (Int, String)) ~?= (Right 7, Right "hello")
      ]

-- Test cases for assocPair
testAssocPair :: Test
testAssocPair = "assocPair" ~:
  testIso assocPair (1 :: Integer, (2 :: Integer, 3)) ((1 :: Integer, 2), 3 :: Integer)

-- Test cases for assocEither
testAssocEither :: Test
testAssocEither = "assocEither" ~:
  TestList
    [ testIso assocEither (Left 1 :: Either Int (Either Int Int)) (Left (Left 1) :: Either (Either Int Int) Int)
    , testIso assocEither (Right (Left 2) :: Either Int (Either Int Int)) (Left (Right 2) :: Either (Either Int Int) Int)
    , testIso assocEither (Right (Right 3) :: Either Int (Either Int Int)) (Right 3 :: Either (Either Int Int) Int)
    ]

-- Helper functions

-- Helper function to test isomorphisms
testIso :: (Eq a, Eq b, Show a, Show b) => (a <-> b) -> a -> b -> Test
testIso (Iso f g) a b = TestList
  [ "forward" ~: f a ~?= b
  , "backward" ~: g b ~?= a
  ]
