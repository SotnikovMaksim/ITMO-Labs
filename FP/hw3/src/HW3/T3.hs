module HW3.T3
  ( joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW3.T1

-- | Flattens a nested 'Option' into a single 'Option'.
-- If the outer 'Option' is 'None', the result is 'None'.
joinOption :: Option (Option a) -> Option a
joinOption None     = None
joinOption (Some a) = a

-- | Flattens a nested 'Except' into a single 'Except'.
-- If the outer 'Except' is an 'Error', the result is an 'Error'.
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)   = Error e
joinExcept (Success s) = s

-- | Flattens a nested 'Annotated' by combining the annotations using the 'Semigroup' operation.
-- The outer annotation is combined with the inner one using the '<>' operator.
joinAnnotated :: (Semigroup e) => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((value :# annotationA) :# annotationB) = value :# (annotationB <> annotationA)

-- | Concatenates a list of lists into a single list.
joinList :: List (List a) -> List a
joinList Nil           = Nil
joinList (Nil :. rest) = joinList rest
joinList ((current :. innerRest) :. rest) = current :. joinList (innerRest :. rest)

-- | Flattens a nested 'Fun' into a single 'Fun'.
-- It applies the inner function to the input and then applies the result of the outer function to the input.
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun f = F (\input -> unpackFun (unpackFun f input) input)
  where
    unpackFun (F fun) = fun
