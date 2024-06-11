import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

allTests :: Test
allTests =
  TestList
    [ testDistOption,
      testWrapOption,
      testDistPair,
      testWrapPair,
      testDistQuad,
      testWrapQuad,
      testDistAnnotated,
      testWrapAnnotated,
      testDistExcept,
      testWrapExcept,
      testDistPrioritised,
      testWrapPrioritised,
      testDistStream,
      testWrapStream,
      testDistList,
      testWrapList,
      testDistFun,
      testWrapFun
    ]

main :: IO ()
main = runTestTT allTests >>= print
