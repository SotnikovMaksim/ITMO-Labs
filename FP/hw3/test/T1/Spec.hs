import TestSuit
import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)

allTests :: Test
allTests = TestList
  [ testMapOption
  , testMapPair
  , testMapQuad
  , testMapAnnotated
  , testMapExcept
  ]

main :: IO ()
main = runTestTT allTests >>= print
