import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

testSuite :: Test
testSuite =
  TestList
    [ testNewCHT
    , testGetCHT
    , testPutCHT
    , testSizeCHT
    ]

main :: IO ()
main = runTestTT testSuite >>= print
