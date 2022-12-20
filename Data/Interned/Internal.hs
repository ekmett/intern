{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , BangPatterns
           , CPP
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables #-}

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
import Data.Array.Base (unsafeWrite)
import Data.Array.IO (IOArray, newArray_)
#if MIN_VERSION_base(4,5,0)
import Data.Array.Unsafe (unsafeFreeze)
#else
import Data.Array.IO (unsafeFreeze)
#endif
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Foldable
#if !(MIN_VERSION_base(4,8,0))
import Data.Traversable
#endif
import qualified Data.HashMap.Strict as HashMap
#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,8,0)
-- These versions had a particularly inefficient implementation of
-- atomicModifyIORef', so we use our own instead.
import Data.IORef hiding (atomicModifyIORef')
#else
import Data.IORef
#endif
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
          $ replicateArrayIO w (newIORef element)

-- | Just like 'Control.Monad.replicateM', but the action is required to be in
-- 'IO' and the result is an 'Array' rather than a list. We specialize this to
-- 'IO' and 'Array' to be sure 'unsafeFreeze' rewrite rules fire.
replicateArrayIO :: forall e. Int -> IO e -> IO (Array Int e)
replicateArrayIO n0 m = do
  mary :: IOArray Int e <- newArray_ (0, n0 - 1)
  go mary 0 n0
  where
    go mary i n
      | i >= n
      = unsafeFreeze mary
      | otherwise
      = do
          e <- m
          unsafeWrite mary i e
          go mary (i + 1) n

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
intern !bt = unsafeDupablePerformIO $ modifyAdvice $ atomicModifyIORef' slot go
  where
  slot = getCache cache ! r
  !dt = describe bt
  !hdt = hash dt
  !wid = cacheWidth dt
  r = hdt `mod` wid
  go cs@(CacheState i m) = case HashMap.lookup dt m of
    Nothing -> let t = identify (wid * i + r) bt in (CacheState (i + 1) (HashMap.insert dt t m), t)
    Just t -> (cs, t)

-- given a description, go hunting for an entry in the cache
recover :: Interned t => Description t -> IO (Maybe t)
recover !dt = do
  CacheState _ m <- readIORef $ getCache cache ! (hash dt `mod` cacheWidth dt)
  return $ HashMap.lookup dt m

#if !MIN_VERSION_base(4,8,0)
-- For base<4.6, we define this because it's otherwise unavailable.
-- For 4.6 <= base < 4.8, we define this because the version in base
-- is gratuitously inefficient.
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref $ \a ->
            case f a of
                v@(a',_) -> a' `seq` v
    b `seq` return b
#endif
