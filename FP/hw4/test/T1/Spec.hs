import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

allTests :: Test
allTests =
  TestList
    [ testMapExceptState
    , testWrapExceptState
    , testJoinExceptState
    , testModifyExceptState
    , testEval
    ]

main :: IO ()
main = runTestTT allTests >>= print
