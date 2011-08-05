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

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.MVar
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)
import System.Mem.Weak

data CacheState t = CacheState 
   {-# UNPACK #-} !Id
   !(HashMap (Description t) (Weak t))

newtype Cache t = Cache { getCache :: MVar (CacheState t) }

cacheSize :: Cache t -> IO Int
cacheSize (Cache t) = do
  CacheState _ m <- readMVar t
  return (HashMap.size m)

mkCache :: Interned t => Cache t
mkCache = result where
  result = Cache $ unsafePerformIO $ newMVar $ CacheState (seedIdentity result) HashMap.empty

type Id = Int

class ( Eq (Description t)
      , Hashable (Description t)
      ) => Interned t where
  data Description t
  type Uninterned t
  describe :: Uninterned t -> Description t 
  identify :: Id -> Uninterned t -> t
  identity :: t -> Id
  seedIdentity :: p t -> Id
  seedIdentity _ = 0
  modifyAdvice :: IO t -> IO t
  modifyAdvice = id
  cache        :: Cache t

class Interned t => Uninternable t where
  unintern :: t -> Uninterned t

intern :: Interned t => Uninterned t -> t
intern !bt = unsafeDupablePerformIO $ modifyAdvice $ modifyMVar (getCache cache) go
  where
  dt = describe bt
  go (CacheState i m) = case HashMap.lookup dt m of
    Nothing -> k i m
    Just wt -> do
      mt <- deRefWeak wt
      case mt of 
        Just t -> return (CacheState i m, t)
        Nothing -> k i m
  k i m = do let t = identify i bt 
             wt <- t `seq` mkWeakPtr t $ Just remove
             return (CacheState (i + 1) (HashMap.insert dt wt m), t)
  remove = modifyMVar_ (getCache cache) $ 
    \ (CacheState i m) -> return $ CacheState i (HashMap.delete dt m)

-- given a description, go hunting for an entry in the cache
recover :: Interned t => Description t -> IO (Maybe t)
recover dt = do
  CacheState _ m <- readMVar $ getCache cache
  case HashMap.lookup dt m of
    Nothing -> return Nothing
    Just wt -> deRefWeak wt
