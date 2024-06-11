import TestSuit
import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)

tests :: Test
tests =
  TestList
    [ testDotString,
      testListPlus,
      testInclusive,
      testFun
    ]

main :: IO ()
main = runTestTT tests >>= print
