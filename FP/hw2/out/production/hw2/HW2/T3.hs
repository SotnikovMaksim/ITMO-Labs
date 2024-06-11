module HW2.T3
  ( epart,
    mcat,
  )
where

import Data.Foldable (fold)

-- | Concatenates the monoidal values inside a list of Maybe values, discarding the Nothing values.
mcat :: (Monoid a) => [Maybe a] -> a
mcat = foldr (\current accumulator -> fold current <> accumulator) mempty

-- | Partitions a list of Either values into two monoidal values, one for Left values and the other for Right values.
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart =
  foldr eitherUnpacker (mempty, mempty)
  where
    eitherUnpacker current (left, right) =
      case current of
        Left  element -> (element <> left, right)
        Right element -> (left, element <> right)
