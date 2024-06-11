import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestSuit

allTests :: Test
allTests =
  TestList
    [ testParseError,
      testParseExpr
    ]

main :: IO ()
main = runTestTT allTests >>= print
