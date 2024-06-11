module TestSuit
  ( testMcat,
    testEpart,
  )
where

import HW2.T3
import Test.HUnit.Base (Test (TestList), (~:), (~=?))

-- Tests for 'mcat' function
testMcat :: Test
testMcat =
  TestList
    [ "mcat with all Just values" ~:
        "abc" ~=? mcat [Just "a", Just "b", Just "c"],
      "mcat with Nothing values" ~:
        "ac" ~=? mcat [Just "a", Nothing, Just "c"]
    ]

-- Tests for 'epart' function
testEpart :: Test
testEpart =
  TestList
    [ "epart with various combinations" ~:
        ("abc", "123") ~=? epart [Left "a", Right "1", Left "b", Right "2", Left "c", Right "3"]
    ]
