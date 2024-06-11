import Test.HUnit.Base (Test (TestList))
import Test.HUnit.Text (runTestTT)
import TestT1
import TestT2
import TestT3

tests =
  TestList
    [ testNextDay,
      testAfterDays,
      testIsWeekend,
      testDaysToParty,
      testNplus,
      testNmult,
      testNsub,
      testNcmp,
      testNFromNatural,
      testNToNum,
      testNEven,
      testNOdd,
      testNdiv,
      testNmod,
      testTSize,
      testTDepth,
      testTMember,
      testTInsert,
      testTFromList
    ]

main = runTestTT tests
