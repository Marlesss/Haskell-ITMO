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

import Control.Concurrent.Classy
import Control.Monad
import Data.Array.MArray
import Data.Hashable
import Data.Foldable
--import Debug.Trace

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: (MonadConc m) => m (CHT (STM m) k v)
newCHT = atomically $ do
  bucketsArray <- newBucketsArray initCapacity
  buckets <- newTVar bucketsArray
  size <- newTVar 0
  pure $ CHT buckets size

newBucketsArray :: MonadSTM stm => Int -> stm (BucketsArray stm k v)
newBucketsArray capacity = newArray (0, capacity - 1) []

getCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT k (CHT {chtBuckets = bucketsVar, chtSize = _}) = atomically $ do
  bucketsArray <- readTVar bucketsVar
  capacity <- bucketsArrayCapacity bucketsArray
  bucket <- readArray bucketsArray (hash k `mod` capacity)
  pure $ snd <$> find ((== k) . fst) bucket

bucketsArrayCapacity :: MonadSTM stm => BucketsArray stm k v -> stm Int
bucketsArrayCapacity buckets = rangeSize <$> getBounds buckets

putCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT k v cht@(CHT {chtBuckets = bucketsVar, chtSize = sizeVar}) = atomically $ do
  bucketsArray <- readTVar bucketsVar
  capacity <- bucketsArrayCapacity bucketsArray
  let bucketIndex = hash k `mod` capacity
  bucket <- readArray bucketsArray bucketIndex
  unless (k `elem` map fst bucket) (modifyTVar sizeVar (+ 1))
  let newBucket = (k, v) : filter ((/= k) . fst) bucket
  writeArray bucketsArray bucketIndex newBucket
  size <- readTVar sizeVar
  unless (fromIntegral size < fromIntegral capacity * loadFactor) (resizeCHT cht)

resizeCHT :: (MonadSTM stm, Hashable k) => CHT stm k v -> stm ()
resizeCHT cht = do
  oldBucketsArray <- readTVar $ chtBuckets cht
  oldCapacity <- bucketsArrayCapacity oldBucketsArray
  let capacity = oldCapacity * 2
  bucketsArray <- newBucketsArray capacity
  elems <- getElems oldBucketsArray
  let buckets = concat elems
  mapM_ (putBucketsArrayUnsafe bucketsArray) buckets
  writeTVar (chtBuckets cht) bucketsArray

putBucketsArrayUnsafe :: (MonadSTM stm, Hashable k) => BucketsArray stm k v -> (k, v) -> stm ()
putBucketsArrayUnsafe bucketsArray (k, v) = do
  capacity <- bucketsArrayCapacity bucketsArray
  let bucketIndex = hash k `mod` capacity
  bucket <- readArray bucketsArray bucketIndex
  let newBucket = (k, v) : bucket
  writeArray bucketsArray bucketIndex newBucket

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = readTVarConc . chtSize
