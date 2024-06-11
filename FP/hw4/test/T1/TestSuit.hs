module TestSuit
  ( testMapExceptState,
    testWrapExceptState,
    testJoinExceptState,
    testModifyExceptState,
    testEval
  )
where

import HW4.T1
import HW4.Types
import Test.HUnit.Base (Test (TestList), (~:), (~?=))

-- | Tests for mapExceptState
testMapExceptState :: Test
testMapExceptState =
  TestList
    [ "mapExceptState should apply a function to the result" ~:
        let testAction = mapExceptState (+ 1) (wrapExceptState 3 :: ExceptState () Int Int)
        in runExceptStateTest testAction (0 :: Int) ~?= (Right 4, 0),
      "mapExceptState should not change the state" ~:
        let testAction = mapExceptState (* 2) (wrapExceptState 10 :: ExceptState () Int Int)
        in runExceptStateTest testAction (100 :: Int) ~?= (Right 20, 100)
    ]

-- | Tests for wrapExceptState
testWrapExceptState :: Test
testWrapExceptState =
  TestList
    [ "wrapExceptState should wrap a value without changing the state" ~:
        runExceptStateTest (wrapExceptState 42 :: ExceptState () Int Int) (0 :: Int) ~?= (Right 42, 0),
      "wrapExceptState should keep the initial state intact" ~:
        runExceptStateTest (wrapExceptState 'a' :: ExceptState () Char Char) 'z' ~?= (Right 'a', 'z')
    ]

-- | Tests for joinExceptState
testJoinExceptState :: Test
testJoinExceptState =
  TestList
    [ "joinExceptState should flatten nested ExceptState" ~:
        let innerExceptState :: ExceptState EvaluationError Char Char
            innerExceptState = wrapExceptState 'x'
            outerExceptState :: ExceptState EvaluationError Char (ExceptState EvaluationError Char Char)
            outerExceptState = wrapExceptState innerExceptState
         in runExceptStateTest (joinExceptState outerExceptState) 'z' ~?= (Right 'x', 'z'),
      "joinExceptState should update except state correctly" ~:
        let innerExceptState :: ExceptState EvaluationError Char Char
            innerExceptState = modifyExceptState (const 'y') >> wrapExceptState 'x'
            outerExceptState :: ExceptState EvaluationError Char (ExceptState EvaluationError Char Char)
            outerExceptState = wrapExceptState innerExceptState
         in runExceptStateTest (joinExceptState outerExceptState) 'z' ~?= (Right 'x', 'y')
    ]

-- | Tests for modifyExceptState
testModifyExceptState :: Test
testModifyExceptState =
  TestList
    [ "modifyExceptState should apply a function to the except state" ~:
        runExceptStateTest (modifyExceptState (+ 1) :: ExceptState () Int ()) (0 :: Int) ~?= (Right (), 1),
      "modifyExceptState should be able to use the previous except state" ~:
        runExceptStateTest (modifyExceptState (\x -> x * x) :: ExceptState () Int ()) (3 :: Int) ~?= (Right (), 9)
    ]

-- | Tests for eval
testEval :: Test
testEval =
  TestList
    [ "eval Val should return the value without logging" ~:
        runExceptStateTest (eval (Val 10)) [] ~?= (Right 10.0, []),
      "eval Add should perform addition and log the operation" ~:
        let expr = Op (Add (Val 2) (Val 3))
            expectedLog = [Add 2.0 3.0]
         in runExceptStateTest (eval expr) [] ~?= (Right 5.0, expectedLog),
      "eval Sub should perform subtraction and log the operation" ~:
        let expr = Op (Sub (Val 5) (Val 2))
            expectedLog = [Sub 5.0 2.0]
         in runExceptStateTest (eval expr) [] ~?= (Right 3.0, expectedLog),
      "eval Mul should perform multiplication and log the operation" ~:
        let expr = Op (Mul (Val 7) (Val 6))
            expectedLog = [Mul 7.0 6.0]
         in runExceptStateTest (eval expr) [] ~?= (Right 42.0, expectedLog),
      "eval Div should perform division and log the operation" ~:
        let expr = Op (Div (Val 10) (Val 2))
            expectedLog = [Div 10.0 2.0]
         in runExceptStateTest (eval expr) [] ~?= (Right 5.0, expectedLog),
      "eval Abs should perform abs and log the operation" ~:
        let expr = Op (Abs (Val (-3)))
            expectedLog = [Abs (-3.0)]
         in runExceptStateTest (eval expr) [] ~?= (Right 3.0, expectedLog),
      "eval Sgn should perform signum and log the operation" ~:
        let exprPos = Op (Sgn (Val 3))
            exprNeg = Op (Sgn (Val (-3)))
            expectedLogPos = [Sgn 3.0]
            expectedLogNeg = [Sgn (-3.0)]
         in TestList
              [ runExceptStateTest (eval exprPos) [] ~?= (Right 1.0, expectedLogPos),
                runExceptStateTest (eval exprNeg) [] ~?= (Right (-1.0), expectedLogNeg)
              ]
    ]

-- Helper functions

-- | Helper to run an ExceptState and extract the result, state, and possible error.
runExceptStateTest :: ExceptState e s a -> s -> (Either e a, s)
runExceptStateTest state initialState =
  case runES state initialState of
    Error err                    -> (Left err, initialState)
    Success (result :# newState) -> (Right result, newState)


