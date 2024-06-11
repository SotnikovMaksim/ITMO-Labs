module TestT1
  ( testNextDay,
    testAfterDays,
    testIsWeekend,
    testDaysToParty,
  )
where

import HW1.T1
import Test.HUnit.Base (Test (TestList), (~?=))

-- Тесты для функции nextDay
testNextDay :: Test
testNextDay =
  TestList
    [ nextDay Monday ~?= Tuesday,
      nextDay Tuesday ~?= Wednesday,
      nextDay Wednesday ~?= Thursday,
      nextDay Thursday ~?= Friday,
      nextDay Friday ~?= Saturday,
      nextDay Saturday ~?= Sunday,
      nextDay Sunday ~?= Monday
    ]

-- Тесты для функции afterDays
testAfterDays :: Test
testAfterDays =
  TestList
    [ afterDays 0 Monday ~?= Monday,
      afterDays 1 Monday ~?= Tuesday,
      afterDays 7 Monday ~?= Monday,
      afterDays 10 Monday ~?= Thursday,
      afterDays 14 Monday ~?= Monday
    ]

-- Тесты для функции isWeekend
testIsWeekend :: Test
testIsWeekend =
  TestList
    [ isWeekend Saturday ~?= True,
      isWeekend Sunday ~?= True,
      isWeekend Monday ~?= False,
      isWeekend Wednesday ~?= False
    ]

-- Тесты для функции daysToParty
testDaysToParty :: Test
testDaysToParty =
  TestList
    [ daysToParty Monday ~?= 4,
      daysToParty Tuesday ~?= 3,
      daysToParty Friday ~?= 0
    ]
