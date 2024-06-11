module TestSuit
  ( testSplitOn,
    testJoinWith,
  )
where

import HW2.T2
import Data.List.NonEmpty (NonEmpty (..))
import Test.HUnit.Base (Test (TestList), (~:), (~=?))

-- Tests for 'splitOn' function
testSplitOn :: Test
testSplitOn =
  TestList
    [ "splitOn with basic case" ~:
        ("a" :| ["b", "c"]) ~=? splitOn ',' "a,b,c",
      "splitOn with consecutive separators" ~:
        ("a" :| ["", "b"]) ~=? splitOn ',' "a,,b"
    ]

-- Tests for 'joinWith' function
testJoinWith :: Test
testJoinWith =
  TestList
    [ "joinWith basic case" ~:
        "a.b.c" ~=? joinWith '.' ("a" :| ["b", "c"])
    ]
