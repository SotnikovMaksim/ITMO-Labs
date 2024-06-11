module TestSuit
  ( testParseError,
    testParseExpr,
  )
where

import HW4.T2
import HW4.Types
import Test.HUnit.Base (Test (TestCase, TestList), assertBool, assertFailure, (~:), (~?=))

-- | Tests for parseError
testParseError :: Test
testParseError =
  TestList
    [ "parseError should result in an error state" ~:
        let result = runP parseError ""
         in case result of
              Error (ErrorAtPos pos) -> assertBool "Position should be 0" (pos == 0)
              _ -> assertFailure "Expected parse error"
    ]

-- | Tests for parseExpr
testParseExpr :: Test
testParseExpr =
  TestList
    [ "parseExpr should correctly parse a simple value" ~:
        parseExpr "42"
          ~?= Success (Val 42.0),
      "parseExpr should handle addition" ~:
        parseExpr "3 + 5"
          ~?= Success (Op (Add (Val 3.0) (Val 5.0))),
      "parseExpr should handle subtraction" ~:
        parseExpr "10 - 4"
          ~?= Success (Op (Sub (Val 10.0) (Val 4.0))),
      "parseExpr should handle multiplication" ~:
        parseExpr "6 * 7"
          ~?= Success (Op (Mul (Val 6.0) (Val 7.0))),
      "parseExpr should handle division" ~:
        parseExpr "8 / 2"
          ~?= Success (Op (Div (Val 8.0) (Val 2.0))),
      "parseExpr should handle nested expressions" ~:
        parseExpr "(3 + 5) * 2"
          ~?= Success (Op (Mul (Op (Add (Val 3.0) (Val 5.0))) (Val 2.0))),
      "parseExpr should handle deeply nested expressions" ~:
        parseExpr "((1 + 2) * 3) / (4 - 5)"
          ~?= Success (Op (Div (Op (Mul (Op (Add (Val 1.0) (Val 2.0))) (Val 3.0))) (Op (Sub (Val 4.0) (Val 5.0))))),
      "parseExpr should handle whitespace" ~:
        parseExpr "  1   + 2 "
          ~?= Success (Op (Add (Val 1.0) (Val 2.0))),
      "parseExpr should handle whitespace" ~:
        parseExpr "  1   + 2 "
          ~?= Success (Op (Add (Val 1.0) (Val 2.0))),
      "parseExpr should return error for invalid input" ~:
        let result = parseExpr "invalid"
         in case result of
              Error _ -> TestCase (assertBool "Expected parse error" True)
              _ -> TestCase (assertFailure "Expected parse error"),
      "parseExpr should return error for incomplete expression" ~:
        let result = parseExpr "(3 + "
         in case result of
              Error _ -> TestCase (assertBool "Expected parse error" True)
              _ -> TestCase (assertFailure "Expected parse error"),
      "parseExpr should return error for unbalanced parentheses" ~:
        let result = parseExpr "(1 + (2 - 3))"
         in case result of
              Error _ -> TestCase (assertBool "Expected parse error" True)
              _ -> TestCase (assertFailure "Expected parse error")
    ]
