{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , BangPatterns
           , GeneralizedNewtypeDeriving #-}

module Data.Interned.Internal
  ( Interned(..)
  , Uninternable(..)
  , mkCache
  , Cache(..)
  , CacheState(..)
  , cacheSize
  , Id
  , intern
  , recover
  ) where

import Data.Array
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Foldable
import Data.Traversable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)

-- tuning parameter
defaultCacheWidth :: Int
defaultCacheWidth = 1024

data CacheState t = CacheState
   { fresh :: {-# UNPACK #-} !Id
   , content :: !(HashMap (Description t) t)
   }

newtype Cache t = Cache { getCache :: Array Int (IORef (CacheState t)) }

cacheSize :: Cache t -> IO Int
cacheSize (Cache t) = foldrM
   (\a b -> do
       v <- readIORef a
       return $! HashMap.size (content v) + b
   ) 0 t

mkCache :: Interned t => Cache t
mkCache   = result where
  element = CacheState (seedIdentity result) HashMap.empty
  w       = cacheWidth result
  result  = Cache
          $ unsafePerformIO
          $ traverse newIORef
          $ listArray (0,w - 1)
          $ replicate w element

type Id = Int

class ( Eq (Description t)
      , Hashable (Description t)
      ) => Interned t where
  data Description t
  type Uninterned t
  describe :: Uninterned t -> Description t
  identify :: Id -> Uninterned t -> t
  -- identity :: t -> Id
  seedIdentity :: p t -> Id
  seedIdentity _ = 0
  cacheWidth :: p t -> Int
  cacheWidth _ = defaultCacheWidth
  modifyAdvice :: IO t -> IO t
  modifyAdvice = id
  cache        :: Cache t

class Interned t => Uninternable t where
  unintern :: t -> Uninterned t

intern :: Interned t => Uninterned t -> t
intern !bt = unsafeDupablePerformIO $ modifyAdvice $ atomicModifyIORef slot go
  where
  slot = getCache cache ! r
  !dt = describe bt
  !hdt = hash dt
  !wid = cacheWidth dt
  r = hdt `mod` wid
  go (CacheState i m) = case HashMap.lookup dt m of
    Nothing -> let t = identify (wid * i + r) bt in (CacheState (i + 1) (HashMap.insert dt t m), t)
    Just t -> (CacheState i m, t)

-- given a description, go hunting for an entry in the cache
recover :: Interned t => Description t -> IO (Maybe t)
recover !dt = do
  CacheState _ m <- readIORef $ getCache cache ! (hash dt `mod` cacheWidth dt)
  return $ HashMap.lookup dt m
