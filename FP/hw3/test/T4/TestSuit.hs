module TestSuit
  ( testMapState,
    testWrapState,
    testJoinState,
    testModifyState,
    testEval,
  )
where

import HW3.T1
import HW3.T4
import Test.HUnit.Base (Test (TestList), (~:), (~?=))

-- | Tests for mapState
testMapState :: Test
testMapState =
  TestList
    [ "mapState should apply a function to the result" ~:
        runStateTest (mapState (+ 1) (wrapState 3)) (0 :: Int) ~?= (4 :: Int, 0),
      "mapState should not change the state" ~:
        runStateTest (mapState (* 2) (wrapState 10)) (100 :: Int) ~?= (20 :: Int, 100)
    ]

-- | Tests for wrapState
testWrapState :: Test
testWrapState =
  TestList
    [ "wrapState should wrap a value without changing the state" ~:
        runStateTest (wrapState 42) (0 :: Int) ~?= (42 :: Int, 0),
      "wrapState should keep the initial state intact" ~:
        runStateTest (wrapState 'a') 'z' ~?= ('a', 'z')
    ]

-- | Tests for joinState
testJoinState :: Test
testJoinState =
  TestList
    [ "joinState should flatten nested State" ~:
        let innerState = wrapState 'x'
            outerState = wrapState innerState
         in runStateTest (joinState outerState) 'z' ~?= ('x', 'z'),
      "joinState should update state correctly" ~:
        let innerState = modifyState (const 'y') >> wrapState 'x'
            outerState = wrapState innerState
         in runStateTest (joinState outerState) 'z' ~?= ('x', 'y')
    ]

-- | Tests for modifyState
testModifyState :: Test
testModifyState =
  TestList
    [ "modifyState should apply a function to the state" ~:
        runStateTest (modifyState (+ 1)) (0 :: Int) ~?= ((), 1),
      "modifyState should be able to use the previous state" ~:
        runStateTest (modifyState (\x -> x * x)) (3 :: Int) ~?= ((), 9)
    ]

-- | Tests for eval
testEval :: Test
testEval =
  TestList
    [ "eval Val should return the value without logging" ~:
        runStateTest (eval (Val 10)) [] ~?= (10, []),
      "eval Add should perform addition and log the operation" ~:
        let expr = Op (Add (Val 2) (Val 3))
            expectedLog = [Add 2.0 3.0]
         in runStateTest (eval expr) [] ~?= (5, expectedLog),
      "eval Sub should perform subtraction and log the operation" ~:
        let expr = Op (Sub (Val 5) (Val 2))
            expectedLog = [Sub 5.0 2.0]
         in runStateTest (eval expr) [] ~?= (3, expectedLog),
      "eval Mul should perform multiplication and log the operation" ~:
        let expr = Op (Mul (Val 7) (Val 6))
            expectedLog = [Mul 7.0 6.0]
         in runStateTest (eval expr) [] ~?= (42, expectedLog),
      "eval Div should perform division and log the operation" ~:
        let expr = Op (Div (Val 10) (Val 2))
            expectedLog = [Div 10.0 2.0]
         in runStateTest (eval expr) [] ~?= (5, expectedLog),
      "eval Abs should perform abs and log the operation" ~:
        let expr = Op (Abs (Val (-3)))
            expectedLog = [Abs (-3.0)]
         in runStateTest (eval expr) [] ~?= (3, expectedLog),
      "eval Sgn should perform signum and log the operation" ~:
        let exprPos = Op (Sgn (Val 3))
            exprNeg = Op (Sgn (Val (-3)))
            expectedLogPos = [Sgn 3.0]
            expectedLogNeg = [Sgn (-3.0)]
         in TestList
              [ runStateTest (eval exprPos) [] ~?= (1, expectedLogPos),
                runStateTest (eval exprNeg) [] ~?= (-1, expectedLogNeg)
              ]
    ]

-- Helper functions

-- | Helper to run a State and extract the result and state.
runStateTest :: State s a -> s -> (a, s)
runStateTest state initialState = let (result :# newState) = runS state initialState in (result, newState)
