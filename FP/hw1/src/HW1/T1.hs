module HW1.T1
  ( Day (..),
    afterDays,
    daysToParty,
    isWeekend,
    nextDay,
  )
where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq)

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay day =
  case day of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays daysPassed currentDay =
  case daysPassed `mod` 7 of
    0 -> currentDay
    _ -> afterDays (daysPassed `mod` 7 - 1) (nextDay currentDay)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend currentDay =
  case currentDay of
    Saturday -> True
    Sunday   -> True
    _        -> False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty currentDay =
  case currentDay of
    Friday -> 0
    _      -> daysToParty (nextDay currentDay) + 1
