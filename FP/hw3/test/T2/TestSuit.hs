module TestSuit
  ( testDistOption,
    testWrapOption,
    testDistPair,
    testWrapPair,
    testDistQuad,
    testWrapQuad,
    testDistAnnotated,
    testWrapAnnotated,
    testDistExcept,
    testWrapExcept,
    testDistPrioritised,
    testWrapPrioritised,
    testDistStream,
    testWrapStream,
    testDistList,
    testWrapList,
    testDistFun,
    testWrapFun,
  )
where

import HW3.T1
import HW3.T2
import Test.HUnit.Base (Test (TestList), (~:), (~?=))

-- | Tests for 'distOption'
testDistOption :: Test
testDistOption =
  TestList
    [ "distOption with two Somes" ~:
        distOption (Some (1 :: Int), Some ("a" :: String))
          ~?= Some (1, "a"),
      "distOption with None as first parameter" ~:
        distOption (None :: Option Int, Some "a")
          ~?= (None :: Option (Int, String)),
      "distOption with None as second parameter" ~:
        distOption (Some (1 :: Int), None :: Option String)
          ~?= (None :: Option (Int, String))
    ]

-- | Tests for 'wrapOption'
testWrapOption :: Test
testWrapOption =
  TestList
    [ "wrapOption with integer" ~: wrapOption (1 :: Int) ~?= Some 1,
      "wrapOption with string" ~: wrapOption "hello" ~?= Some "hello"
    ]

-- | est for 'distPair'
testDistPair :: Test
testDistPair =
  TestList
    [ "distPair with integers" ~:
        distPair (P (1 :: Int) 2, P (3 :: Int) 4)
          ~?= P (1, 3) (2, 4)
    ]

-- | Tests for 'wrapPair'
testWrapPair :: Test
testWrapPair =
  TestList
    [ "wrapPair with integer" ~: wrapPair (1 :: Int) ~?= P 1 1
    ]

-- | Tests for 'distQuad'
testDistQuad :: Test
testDistQuad =
  TestList
    [ "distQuad with integers" ~:
        distQuad (Q (1 :: Int) 2 3 4, Q (5 :: Int) 6 7 8)
          ~?= Q (1, 5) (2, 6) (3, 7) (4, 8)
    ]

-- | Tests for 'wrapQuad'
testWrapQuad :: Test
testWrapQuad =
  TestList
    [ "wrapQuad with integer" ~:
        wrapQuad (1 :: Int)
          ~?= Q 1 1 1 1
    ]

-- | Tests for 'distAnnotated'
testDistAnnotated :: Test
testDistAnnotated =
  TestList
    [ "distAnnotated with strings" ~:
        distAnnotated ("a" :# "x", "b" :# "y")
          ~?= (("a", "b") :# "xy")
    ]

-- | Tests for 'wrapAnnotated'
testWrapAnnotated :: Test
testWrapAnnotated =
  TestList
    [ "wrapAnnotated with integer" ~: wrapAnnotated (1 :: Int) ~?= (1 :# "")
    ]

-- | Tests for 'distExcept'
testDistExcept :: Test
testDistExcept =
  TestList
    [ "distExcept with Success" ~:
        (distExcept (Success 1, Success "a") :: Except String (Int, String))
          ~?= Success ((1, "a") :: (Int, String)),
      "distExcept with Error first" ~:
        (distExcept (Error "error", Success "a") :: Except String (Int, String))
          ~?= (Error "error" :: Except String (Int, String)),
      "distExcept with Error second" ~:
        (distExcept (Success 1, Error "error") :: Except String (Int, String))
          ~?= (Error "error" :: Except String (Int, String))
    ]

-- | Tests for 'wrapExcept'
testWrapExcept :: Test
testWrapExcept =
  TestList
    [ "wrapExcept with integer" ~: (wrapExcept 1 :: Except String Int) ~?= Success 1
    ]

-- | Tests for 'distPrioritised'
testDistPrioritised :: Test
testDistPrioritised =
  TestList
    [ "distPrioritised both Low" ~: distPrioritised (Low (1 :: Int), Low ("a" :: String)) ~?= Low (1, "a"),
      "distPrioritised one High" ~: distPrioritised (High (1 :: Int), Low ("a" :: String)) ~?= High (1, "a")
    ]

-- | Tests for 'wrapPrioritised'
testWrapPrioritised :: Test
testWrapPrioritised =
  TestList
    [ "wrapPrioritised with integer" ~: wrapPrioritised (1 :: Int) ~?= Low (1 :: Int)
    ]

-- | Tests for 'distStream'
testDistStream :: Test
testDistStream =
  TestList
    [ "distStream with two finite streams" ~:
        takeStream 5 (distStream (streamFromList [(1 :: Int) .. 5], streamFromList ['a' .. 'e']))
          ~?= takeStream 5 (streamFromList [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]),
      "distStream with infinite streams" ~:
        takeStream 5 (distStream (streamFromList [(1 :: Int) ..], streamFromList ['a' ..]))
          ~?= [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]
    ]

-- | Tests for 'wrapStream'
testWrapStream :: Test
testWrapStream =
  TestList
    [ "wrapStream with an integer" ~:
        takeStream 5 (wrapStream (42 :: Int))
          ~?= replicate 5 42,
      "wrapStream with a character" ~:
        takeStream 4 (wrapStream 'c')
          ~?= replicate 4 'c',
      "wrapStream with a string" ~:
        takeStream 3 (wrapStream "hello")
          ~?= replicate 3 "hello",
      "wrapStream with a boolean" ~:
        takeStream 6 (wrapStream True)
          ~?= replicate 6 True
    ]

-- | Tests for 'distList'
testDistList :: Test
testDistList =
  TestList
    [ "distList with two finite lists of equal length" ~:
        takeList 5 (distList (fromStandardList [(1 :: Int) .. 3], fromStandardList ['a' .. 'c']))
          ~?= [(1, 'a'), (1, 'b'), (1, 'c'), (2, 'a'), (2, 'b')],
      "distList with first list longer than the second" ~:
        takeList 5 (distList (fromStandardList [(1 :: Int) ..], fromStandardList ['a' .. 'c']))
          ~?= [(1, 'a'), (1, 'b'), (1, 'c'), (2, 'a'), (2, 'b')],
      "distList with second list longer than the first" ~:
        takeList 5 (distList (fromStandardList [(1 :: Int) .. 3], fromStandardList ['a' ..]))
          ~?= [(1, 'a'), (1, 'b'), (1, 'c'), (1, 'd'), (1, 'e')]
    ]

-- | Tests for 'wrapList'
testWrapList :: Test
testWrapList =
  TestList
    [ "wrapList with integer" ~: wrapList (1 :: Int) ~?= (1 :. Nil)
    ]

-- | Tests for 'distFun'
testDistFun :: Test
testDistFun =
  TestList
    [ "distFun with simple functions" ~: let (F f) = distFun (F (* (2 :: Int)), F (+ 10)) in f 5 ~?= (10, 15)
    ]

-- | Tests for 'wrapFun'
testWrapFun :: Test
testWrapFun =
  TestList
    [ "wrapFun with integer" ~: let (F f) = wrapFun (1 :: Int) in f (999 :: Int) ~?= 1
    ]

-- Helper functions

-- | Take the first 'n' elements of a 'Stream' as a list.
takeStream :: Int -> Stream a -> [a]
takeStream 0 _ = []
takeStream n (x :> xs) = x : takeStream (n - 1) xs

-- | A helper to convert a finite list to an infinite stream
streamFromList :: [a] -> Stream a
streamFromList = foldr (:>) (error "Streams must be infinite")

-- | Convert a standard Haskell list to our custom 'List' type.
fromStandardList :: [a] -> List a
fromStandardList = foldr (:.) Nil

-- | Take the first 'n' elements of a 'List' as a standard Haskell list for comparison.
takeList :: Int -> List a -> [a]
takeList 0 _   = []
takeList _ Nil = []
takeList n (x :. xs) = x : takeList (n - 1) xs
