module TestSuit
  ( testJoinOption,
    testJoinExcept,
    testJoinAnnotated,
    testJoinList,
    testJoinFun,
  )
where

import HW3.T1
import HW3.T3
import Test.HUnit.Base (Test (TestList), (~:), (~?=))

-- | Tests for 'joinOption'
testJoinOption :: Test
testJoinOption =
  TestList
    [ "joinOption with None" ~: joinOption (None :: Option (Option Int)) ~?= None,
      "joinOption with Some None" ~: joinOption (Some None :: Option (Option Int)) ~?= None,
      "joinOption with Some Some" ~: joinOption (Some (Some 42) :: Option (Option Int)) ~?= Some 42
    ]

-- | Tests for 'joinExcept'
testJoinExcept :: Test
testJoinExcept =
  TestList
    [ "joinExcept with Error" ~:
        joinExcept (Error "Outer error" :: Except String (Except String Int))
          ~?= Error "Outer error",
      "joinExcept with Success Error" ~:
        joinExcept (Success (Error "Inner error") :: Except String (Except String Int))
          ~?= Error "Inner error",
      "joinExcept with Success Success" ~:
        joinExcept (Success (Success 42) :: Except String (Except String Int))
          ~?= Success 42
    ]

-- | Tests for 'joinAnnotated'
testJoinAnnotated :: Test
testJoinAnnotated =
  TestList
    [ "joinAnnotated with annotations" ~:
        joinAnnotated (((42 :: Int) :# ("inner" :: String)) :# "outer") ~?= (42 :# "outerinner")
    ]

-- | Tests for 'joinList'
testJoinList :: Test
testJoinList =
  TestList
    [ "joinList with empty lists" ~: joinList ((Nil :: List Int) :. Nil :. Nil :. Nil) ~?= Nil,
      "joinList with non-empty lists" ~:
        joinList (((1 :: Int) :. 2 :. Nil) :. (3 :. Nil) :. Nil) ~?= (1 :. 2 :. 3 :. Nil)
    ]

-- | Tests for 'joinFun'
testJoinFun :: Test
testJoinFun =
  TestList
    [ "joinFun with constant functions" ~:
        applyFun (joinFun (F (\x -> F (x +)))) (5 :: Int) ~?= applyFun (F (5 +)) 5,
      "joinFun with identity and constant" ~:
        applyFun (joinFun (F (\_ -> F id))) (42 :: Int) ~?= applyFun (F id) 42,
      "joinFun with squaring functions" ~:
        applyFun (joinFun (F (\x -> F (x *)))) (3 :: Int) ~?= applyFun (F (3 *)) 3
    ]

-- Helper functions

-- Helper function to apply Fun
applyFun :: Fun i a -> i -> a
applyFun (F fun) = fun
