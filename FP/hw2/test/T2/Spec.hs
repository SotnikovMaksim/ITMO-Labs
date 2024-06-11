import TestSuit
import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)

tests :: Test
tests =
  TestList
    [ testSplitOn,
      testJoinWith
    ]

main :: IO ()
main = runTestTT tests >>= print
