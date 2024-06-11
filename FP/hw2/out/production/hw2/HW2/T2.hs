module HW2.T2
  ( joinWith,
    splitOn,
  )
where

import Data.List.NonEmpty

-- | Split a list by a given separator, ensuring at least one split is present.
splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn separator =
  foldr split ([] :| [])
  where
    split current (first :| rest)
      | current == separator = [] :| (first : rest)
      | otherwise            = (current : first) :| rest

-- | Join a non-empty list of lists using a given separator.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith separator (first :| rest) =
  first ++ foldr (\current accumulator -> separator : current ++ accumulator) [] rest
