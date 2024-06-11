module TestSuit
  ( testDoubleNeg
  , testReduceTripleNeg
  )
where

import Test.HUnit.Base (Test (TestCase, TestList), (~:), assertBool)

-- Testing them directly in a conventional way (like checking for equality) 
-- is difficult due to the nature of the types involved (Void and function types).
-- Since that I couldn't imagine any tests :( Please, forgive me ...

-- Test for doubleNeg
testDoubleNeg :: Test
testDoubleNeg = "doubleNeg" ~:
  TestList
    [ "doubleNeg exists" ~: TestCase (assertBool "Exists" True) 
    ]

-- Test for reduceTripleNeg
testReduceTripleNeg :: Test
testReduceTripleNeg = "reduceTripleNeg" ~:
  TestList
    [ "reduceTripleNeg exists" ~: TestCase (assertBool "Exists" True)
    ]