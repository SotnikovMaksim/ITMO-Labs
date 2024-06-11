{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module TestSuit
  ( testContains,
    testDelete,
    testAdd,
  )
where

import GHC.TypeLits
import HW6.T2
import Test.HUnit
import Type.Reflection( (:~:)( Refl ) )

-- Test Contains
testContains :: Test
testContains =
  "Contains" ~:
    TestList
      [ TestCase $ do
          let result = boolVal (Proxy :: Proxy (Contains "a" '["a", "b"]))
          assertEqual "Contains a in [a, b]" result True,
        TestCase $ do
          let result = boolVal (Proxy :: Proxy (Contains "c" '["a", "b"]))
          assertEqual "Does not contain c in [a, b]" result False
      ]

-- Test Delete
testDelete :: Test
testDelete =
  "Delete" ~:
    TestList
      [ TestCase $ do
          let result = typeListToValue (Proxy :: Proxy (Delete "a" '["a", "b"]))
          assertEqual "Delete a from [a, b]" result ["b"],
        TestCase $ do
          let result = typeListToValue (Proxy :: Proxy (Delete "c" '["a", "b"]))
          assertEqual "Delete c from [a, b] (no change)" result ["a", "b"]
      ]

-- Test Add
testAdd :: Test
testAdd =
  "Add" ~:
    TestList
      [ TestCase $ do
          let result = typeListToValue (Proxy :: Proxy (Add "c" '["a", "b"]))
          assertEqual "Add c to [a, b]" result ["a", "b", "c"],
        TestCase $ do
          let result = typeListToValue (Proxy :: Proxy (Add "a" '["a", "b"]))
          assertEqual "Add a to [a, b] (no change)" result ["a", "b"]
      ]

--------------------------- [ HELPER FUNCTIONS ] ---------------------------

data Proxy a = Proxy

-- Helper function to convert type-level booleans to value-level booleans
boolVal :: forall b. KnownBool b => Proxy b -> Bool
boolVal _ = case boolVal' (Proxy :: Proxy b) of
              Nothing -> False
              Just _  -> True

class KnownBool b where
  boolVal' :: Proxy b -> Maybe (b :~: 'True)

instance KnownBool 'True where
  boolVal' _ = Just Refl

instance KnownBool 'False where
  boolVal' _ = Nothing
  
-- Helper function to convert type-level lists to value-level lists
typeListToValue :: forall (l :: [Symbol]). KnownTypeList l => Proxy l -> [String]
typeListToValue _ = knownTypeListVal (Proxy :: Proxy l)

class KnownTypeList (l :: [Symbol]) where
  knownTypeListVal :: Proxy l -> [String]

instance KnownTypeList '[] where
  knownTypeListVal _ = []

instance (KnownSymbol x, KnownTypeList xs) => KnownTypeList (x ': xs) where
  knownTypeListVal _ = symbolVal (Proxy :: Proxy x) : knownTypeListVal (Proxy :: Proxy xs)