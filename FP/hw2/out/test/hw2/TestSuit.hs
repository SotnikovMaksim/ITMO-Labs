module TestSuit
  ( testDotString,
    testListPlus,
    testInclusive,
    testFun,
  )
where

import HW2.T4
import Test.HUnit.Base (Test (TestList), (~:), (~=?))

-- Tests for 'DotString' function
testDotString :: Test
testDotString =
  TestList
    [ "DotString concatenation" ~:
        DS "a.b" ~=? (DS "a" <> DS "b"),
      "DotString with empty string" ~:
        DS "a" ~=? (DS "a" <> DS "")
    ]

-- Tests for 'ListPlus' function
testListPlus :: Test
testListPlus =
  TestList
    [ "Basic ListPlus concatenation" ~:
        (Last "a" <> Last "b") ~=? ("a" :+ Last "b"),
      "Chaining ListPlus concatenation" ~:
        (Last "a" <> ("b" :+ Last "c")) ~=? ("a" :+ ("b" :+ Last "c"))
    ]

-- Tests for 'Inclusive' function
testInclusive :: Test
testInclusive =
  TestList
    [ "This and This" ~:
        This "ab" ~=? ((This "a" <> This "b") :: Inclusive String String),
      "This and That" ~:
        Both "a" "b" ~=? (This "a" <> That "b"),
      "That and This" ~:
        Both "b" "a" ~=? (That "a" <> This "b"),
      "That and That" ~:
        This "ab" ~=? ((This "a" <> This "b") :: Inclusive String String),
      "This and Both" ~:
        Both "ab" "c" ~=? (This "a" <> Both "b" "c")
    ]

-- Tests for 'Fun' function
testFun :: Test
testFun =
  TestList
    [ "Function composition" ~:
        (8 :: Int)
          ~=? let (F f) = (F (* 2) <> F (+ 1))
               in f 3,
      "Function with identity" ~:
        (5 :: Int)
          ~=? let (F f) = (F id <> F (+ 2))
               in f 3
    ]
