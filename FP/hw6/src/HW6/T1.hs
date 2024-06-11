module HW6.T1
  ( BucketsArray
  , CHT (..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM, MonadConc, atomically)
import Control.Concurrent.Classy.STM (TArray, TVar, newTVar, readTVar, writeTVar)
import Data.Hashable (Hashable, hash)
import Data.Array.MArray (newArray, readArray, writeArray, getElems)
import Control.Monad (when)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  , chtCapacity :: TVar stm Int
  }

-- | Initialize a new Concurrent Hash Table
newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = do
    -- Define the bounds for the array
    let bucketArrayBounds = (0, initCapacity - 1)

    -- Create an empty array of buckets within the defined bounds
    bucketsArray <- atomically $ newArray bucketArrayBounds []

    -- Create a TVar for buckets array
    tBuckets <- atomically $ newTVar bucketsArray

    -- Create a TVar for size
    tSize <- atomically $ newTVar 0
    
    -- Set up number of buckets
    tCapacity <- atomically $ newTVar initCapacity

    return $ CHT tBuckets tSize tCapacity

-- | Retrieve a value for a given key from the CHT
getCHT :: (MonadConc m, Eq k, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT key cht = atomically $ do
    -- Get the buckets array
    bucketsArray <- readTVar $ chtBuckets cht
    
    -- Get buckets capacity
    currentCapacity <- readTVar $ chtCapacity cht

    -- Calculate the index for the key
    let index = hash key `mod` currentCapacity

    -- Get the bucket at the calculated index
    bucket <- readArray bucketsArray index

    -- Search for the key in the bucket and return the corresponding value
    return $ lookup key bucket
    
-- | Insert or update a key-value pair in the CHT
putCHT :: (MonadConc m, Eq k, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT key value cht = do
    -- Inserting the key-value pair and save the new size in order to further check the fullness of the CHT
    newSize <- atomically $ do
          buckets         <- readTVar $ chtBuckets cht
          size            <- readTVar $ chtSize cht
          currentCapacity <- readTVar $ chtCapacity cht
  
          let index = hash key `mod` currentCapacity
          bucket <- readArray buckets index
  
          let (newBucket, isKeyNew) = case lookup key bucket of
                                        Just _  -> (map (\(k,v) -> if k == key then (k,value) else (k,v)) bucket, False)
                                        Nothing -> ((key, value) : bucket, True)
  
          writeArray buckets index newBucket
  
          -- Update the size if the key was new
          let newSize = if isKeyNew then size + 1 else size
          when isKeyNew $
            writeTVar (chtSize cht) newSize
  
          return newSize
  
    currentCapacity <- capacityCHT cht
    -- Check if resizing is needed and perform resizing outside of STM transaction
    when (fromIntegral newSize >= fromIntegral currentCapacity * loadFactor) $ 
      resizeCHT cht
      
-- | Retrieve the current size of the CHT
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = atomically $ do
    readTVar $ chtSize cht
      
--------------------------- [ HELPER FUNCTIONS ] ---------------------------

-- | Retrieve the current buckets count of the CHT
capacityCHT :: MonadConc m => CHT (STM m) k v -> m Int
capacityCHT cht = atomically $ do
    readTVar $ chtCapacity cht

-- | Increasing capacity of CHT
resizeCHT :: (MonadConc m, Hashable k) => CHT (STM m) k v -> m ()
resizeCHT cht = atomically $ do
    -- Read the current capacity
    currentCapacity <- readTVar $ chtCapacity cht

    -- Calculate new capacity (doubling the current capacity)
    let newCapacity = currentCapacity * 2
    newBuckets <- newArray (0, newCapacity - 1) []

    -- Rehash the elements into the new buckets
    oldBuckets <- readTVar $ chtBuckets cht
    oldElems   <- getElems oldBuckets
    let rehashed = concatMap (map (\(k, v) -> (hash k `mod` newCapacity, (k, v)))) oldElems
    mapM_ (\(i, kv) -> do
              bucket <- readArray newBuckets i
              writeArray newBuckets i (kv : bucket)
          ) rehashed

    -- Update the buckets array and the capacity
    writeTVar (chtBuckets cht) newBuckets
    writeTVar (chtCapacity cht) newCapacity
