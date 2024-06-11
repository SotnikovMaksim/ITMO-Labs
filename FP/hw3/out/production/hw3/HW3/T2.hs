module HW3.T2
  ( distOption,
    wrapOption,
    distPair,
    wrapPair,
    distQuad,
    wrapQuad,
    distAnnotated,
    wrapAnnotated,
    distExcept,
    wrapExcept,
    distPrioritised,
    wrapPrioritised,
    distStream,
    wrapStream,
    distList,
    wrapList,
    distFun,
    wrapFun,
  )
where

import HW3.T1

-- | Distribute two 'Option' types into one 'Option' containing a tuple of their values.
-- Returns 'None' if any of the 'Option's is 'None'.
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some a, Some b) = Some (a, b)

-- | Wrap a value in an 'Option', producing a 'Some'.
wrapOption :: a -> Option a
wrapOption = Some

-- | Distribute two 'Pair' types into one 'Pair' containing a tuple of their values.
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

-- | Wrap a value in a 'Pair', duplicating it.
wrapPair :: a -> Pair a
wrapPair a = P a a

-- | Distribute two 'Quad' types into one 'Quad' containing a tuple of their values.
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

-- | Wrap a value in a 'Quad', quadruplicating it.
wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

-- | Distribute two 'Annotated' values into one 'Annotated' containing a tuple of their values,
-- combining their annotations using the semigroup operation.
distAnnotated :: (Semigroup e) => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (valueA :# annotationA, valueB :# annotationB) = (valueA, valueB) :# (annotationA <> annotationB)

-- | Wrap a value in an 'Annotated', with a neutral ('mempty') annotation.
wrapAnnotated :: (Monoid e) => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- | Distribute two 'Except' types into one 'Except' containing a tuple of their values.
-- Returns 'Error' if any of the 'Excepts' is an 'Error'.
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success a, Success b) = Success (a, b)

-- | Wrap a value in a 'Success'.
wrapExcept :: a -> Except e a
wrapExcept = Success

-- | Distribute two 'Prioritised' values into one 'Prioritised' containing a tuple of their values.
-- The resulting priority is the higher of the two input priorities.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (High a, High b)     = High (a, b)

-- | Wrap a value in a 'Prioritised' with the lowest priority 'Low'.
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- | Distribute two 'Stream' values into one 'Stream' containing a tuple of their head elements,
-- followed by the distribution of their tail elements.
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (headFirst :> tailFirst, headSecond :> tailSecond) =
  (headFirst, headSecond) :> distStream (tailFirst, tailSecond)

-- | Wrap a value in a 'Stream', creating an infinite stream of that value.
wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

-- | Distribute two 'List' values into one 'List' containing a tuple of their elements.
-- The resulting list is as long as the shorter of the two input lists.
distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (headFirstList :. tailFirstList, secondList) =
  pairWithFirst headFirstList secondList `appendList` distList (tailFirstList, secondList)
  where
    pairWithFirst :: a -> List b -> List (a, b)
    pairWithFirst _ Nil = Nil
    pairWithFirst element (current :. rest) =
      (element, current) :. pairWithFirst element rest

    appendList :: List a -> List a -> List a
    appendList Nil second = second
    appendList (current :. rest) second =
      current :. appendList rest second

-- | Wrap a value in a 'List', creating a single-element list.
wrapList :: a -> List a
wrapList a = a :. Nil

-- | Distribute two 'Fun' values into one 'Fun' containing a tuple of their results.
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\input -> (f input, g input))

-- | Wrap a value in a 'Fun', creating a constant function.
wrapFun :: a -> Fun i a
wrapFun a = F (const a)
