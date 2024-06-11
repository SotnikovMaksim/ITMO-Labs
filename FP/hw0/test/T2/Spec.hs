import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

testSuite :: Test
testSuite = TestList
  [ testDoubleNeg
  , testReduceTripleNeg
  ]

main :: IO ()
main = runTestTT testSuite >>= print
