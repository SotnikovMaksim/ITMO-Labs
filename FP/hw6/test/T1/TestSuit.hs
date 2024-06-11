module TestSuit
  ( testNewCHT,
    testGetCHT,
    testPutCHT,
    testSizeCHT,
  )
where

import Control.Concurrent.Classy (STM)
import Data.Hashable (Hashable)
import HW6.T1
import Test.HUnit.Base (Test (TestList, TestCase), (~:), assertEqual)

-- Test for 'newCHT'
testNewCHT :: Test
testNewCHT =
  "newCHT" ~:
    TestList
      [ TestCase $ do
          sizeResult <- sizeCHTTest newCHT
          assertEqual "" 0 sizeResult
      ]

-- Test for 'getCHT'
testGetCHT :: Test
testGetCHT =
  "getCHT" ~:
    TestList
      [ TestCase $ do
          result <- getCHTTest (1 :: Int) newCHT
          assertEqual "" (Nothing :: Maybe Int) result,
        TestCase $ do
          result <- getAfterPutTest (1 :: Int) (10 :: Int) newCHT
          assertEqual "" (Just 10) result,
        TestCase $ do
          result <- getAfterPutTest (2 :: Int) (20 :: Int) newCHT
          assertEqual "" (Just 20) result,
        TestCase $ do
          result <- getCHTTest (1 :: Int) newCHT
          assertEqual "Empty table should return Nothing" (Nothing :: Maybe Int) result,
        TestCase $ do
          chTable <- newCHT
          _ <- putCHT (1 :: Int) (10 :: Int) chTable
          _ <- putCHT (2 :: Int) (20 :: Int) chTable
          result <- getCHT (1 :: Int) chTable
          assertEqual "Should retrieve value for key 1" (Just 10) result,
        TestCase $ do
          chTable <- newCHT
          _ <- putCHT (1 :: Int) (10 :: Int) chTable
          _ <- putCHT (2 :: Int) (20 :: Int) chTable
          result <- getCHT (2 :: Int) chTable
          assertEqual "Should retrieve value for key 2" (Just 20) result,
        TestCase $ do
          chTable <- newCHT
          _ <- putCHT (1 :: Int) (10 :: Int) chTable
          _ <- putCHT (1 :: Int) (15 :: Int) chTable
          result <- getCHT (1 :: Int) chTable
          assertEqual "Should retrieve updated value for key 1" (Just 15) result,
        TestCase $ do
          chTable <- newCHT
          _ <- putCHT (1 :: Int) (10 :: Int) chTable
          _ <- putCHT (2 :: Int) (20 :: Int) chTable
          _ <- putCHT (1 :: Int) (15 :: Int) chTable
          result <- getCHT (1 :: Int) chTable
          assertEqual "Should retrieve updated value for key 1 with multiple pairs" (Just 15) result,
        TestCase $ do
          chTable <- newCHT
          _ <- putCHT (1 :: Int) (10 :: Int) chTable
          _ <- putCHT (2 :: Int) (20 :: Int) chTable
          result <- getCHT (3 :: Int) chTable
          assertEqual "Should return Nothing for non-existent key" Nothing result,
        TestCase $ do
          chTable <- newCHT
          -- Insert 20 unique key-value pairs
          mapM_ (\k -> putCHT k (k * 10) chTable) [1..20 :: Int]
          -- Check each pair
          results <- mapM (`getCHT` chTable) [1..20 :: Int]
          let expectedResults = map (Just . (* 10)) [1..20 :: Int]
          assertEqual "Should retrieve correct values for keys 1 to 20" expectedResults results
      ]

-- Test for 'putCHT'
testPutCHT :: Test
testPutCHT =
  "putCHT" ~:
    TestList
      [ TestCase $ do 
          result <- sizeAfterPutTest (1 :: Int) (10 :: Int) newCHT
          assertEqual "" 1 result,
        TestCase $ do
          result <- sizeAfterPutTest (2 :: Int) (20 :: Int) newCHT
          assertEqual "" 1 result
      ]

-- Test for 'sizeCHT'
testSizeCHT :: Test
testSizeCHT =
  "sizeCHT" ~:
    TestList
      [ TestCase $ do 
          result <- sizeCHTTest newCHT
          assertEqual "" 0 result,
        TestCase $ do
          result <- sizeAfterPutTest (1 :: Int) (10 :: Int) newCHT
          assertEqual "" 1 result,
        TestCase $ do
          result <- sizeAfterPutTest (2 :: Int) (20 :: Int) newCHT
          assertEqual "" 1 result,
        TestCase $ do
          cht <- newCHT
          _ <- putCHT (1 :: Int) (10 :: Int) cht
          _ <- putCHT (2 :: Int) (20 :: Int) cht
          result <- sizeCHT cht
          assertEqual "" 2 result,
        TestCase $ do
          cht <- newCHT
          _ <- putCHT (1 :: Int) (10 :: Int) cht
          _ <- putCHT (1 :: Int) (20 :: Int) cht
          _ <- putCHT (1 :: Int) (30 :: Int) cht
          result <- sizeCHT cht
          assertEqual "" 1 result,
        TestCase $ do
          cht <- newCHT
          -- Insert 20 unique key-value pairs
          mapM_ (\k -> putCHT k k cht) [1..20 :: Int]
          result <- sizeCHT cht
          assertEqual "Should be size 20 after adding 20 pairs" 20 result,
        TestCase $ do
          cht <- newCHT
          -- Insert the same key multiple times
          mapM_ (\k -> putCHT (1 :: Int) k cht) [1..20 :: Int]
          result <- sizeCHT cht
          assertEqual "Size should be 1 after adding same key 20 times" 1 result
      ]

--------------------------- [ HELPER FUNCTIONS ] ---------------------------

getCHTTest :: (Eq k, Hashable k) => k -> IO (CHT (STM IO) k v) -> IO (Maybe v)
getCHTTest key chtIO = do
  cht <- chtIO
  getCHT key cht

getAfterPutTest :: (Eq k, Hashable k) => k -> v -> IO (CHT (STM IO) k v) -> IO (Maybe v)
getAfterPutTest key value chtIO = do
  cht <- chtIO
  putCHT key value cht
  getCHT key cht

sizeCHTTest :: IO (CHT (STM IO) k v) -> IO Int
sizeCHTTest chtIO = do
  cht <- chtIO
  sizeCHT cht

sizeAfterPutTest :: (Eq k, Hashable k) => k -> v -> IO (CHT (STM IO) k v) -> IO Int
sizeAfterPutTest key value chtIO = do
  cht <- chtIO
  putCHT key value cht
  sizeCHT cht
