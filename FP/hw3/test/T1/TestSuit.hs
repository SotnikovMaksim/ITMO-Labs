module TestSuit
  ( testMapOption,
    testMapPair,
    testMapQuad,
    testMapAnnotated,
    testMapExcept,
  )
where

import HW3.T1
import Test.HUnit.Base (Test (TestList), (~:), (~?=))

-- Test cases for Option
testMapOption :: Test
testMapOption =
  TestList
    [ "mapOption on None" ~: mapOption (+ 1) (None :: Option Int) ~?= None,
      "mapOption on Some" ~: mapOption (+ 1) (Some (2 :: Int)) ~?= Some 3
    ]

-- Test cases for Pair
testMapPair :: Test
testMapPair =
  TestList
    [ "mapPair" ~: mapPair (+ 1) (P (2 :: Int) 3) ~?= P 3 4
    ]

-- Test cases for Quad
testMapQuad :: Test
testMapQuad =
  TestList
    [ "mapQuad" ~: mapQuad (+ 1) (Q (1 :: Int) 2 3 4) ~?= Q 2 3 4 5
    ]

-- Test cases for Annotated
testMapAnnotated :: Test
testMapAnnotated =
  TestList
    [ "mapAnnotated" ~: mapAnnotated (+ 1) ((5 :: Int) :# "metadata") ~?= (6 :# "metadata")
    ]

-- Test cases for Except
testMapExcept :: Test
testMapExcept =
  TestList
    [ "mapExcept on Error" ~:
        mapExcept (+ 1) (Error ("error" :: String))
          ~?= (Error "error" :: Except String Int),
      "mapExcept on Success" ~:
        mapExcept (+ 1) (Success (5 :: Int))
          ~?= (Success 6 :: Except String Int)
    ]
