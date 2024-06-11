import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

testSuite :: Test
testSuite =
  TestList
    [ testContains
    , testDelete
    , testAdd
    ]

main :: IO ()
main = runTestTT testSuite >>= print
