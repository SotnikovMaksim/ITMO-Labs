import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

testSuite :: Test
testSuite = TestList
  [
  testK
  , testI
  , testCompose
  , testContract
  , testPermute
  ]

main :: IO ()
main = runTestTT testSuite >>= print
