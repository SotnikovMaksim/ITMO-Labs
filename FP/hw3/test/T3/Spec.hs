import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

allTests :: Test
allTests =
  TestList
    [ testJoinOption,
      testJoinExcept,
      testJoinAnnotated,
      testJoinList,
      testJoinFun
    ]

main :: IO ()
main = runTestTT allTests >>= print
