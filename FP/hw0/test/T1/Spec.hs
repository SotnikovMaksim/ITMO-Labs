import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

testSuite :: Test
testSuite = TestList
  [ testFlipIso
  , testRunIso
  , testDistrib
  , testAssocPair
  , testAssocEither
  ]

main :: IO ()
main = runTestTT testSuite >>= print
