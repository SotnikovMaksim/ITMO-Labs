import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

allTests :: Test
allTests =
  TestList
    [ testMapState,
      testWrapState,
      testJoinState,
      testModifyState,
      testEval
    ]

main :: IO ()
main = runTestTT allTests >>= print
