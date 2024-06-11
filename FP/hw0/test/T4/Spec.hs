import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

testSuite :: Test
testSuite = TestList
  [ testRepeat
  , testMap
  , testFib
  , testFac
  ]

main :: IO ()
main = runTestTT testSuite >>= print
