{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , GeneralizedNewtypeDeriving #-}
module Data.Interned.Internal
  ( Interned(..)
  , mkCache
  , Cache(..)
  , CacheState(..)
  , Id(..)
  , intern
  ) where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.MVar
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)
import System.Mem.Weak

data CacheState t = CacheState {-# UNPACK #-} !(Id t) (HashMap (Description t) (Weak t))

newtype Cache t = Cache { getCache :: MVar (CacheState t) }

mkCache :: Cache t
mkCache = Cache $ unsafePerformIO $ newMVar $ CacheState 0 HashMap.empty

newtype Id t = Id Int deriving (Eq,Ord,Show,Num,Real,Integral,Enum)

instance Hashable (Id t) where
  hash (Id t) = hash t
  hashWithSalt s (Id t) = hashWithSalt s t

class ( Eq (Description t)
      , Hashable (Description t)
      ) => Interned t where
  data Description t
  type Uninterned t
  describe :: Uninterned t -> Description t 
  unintern :: t -> Uninterned t
  identify :: Id t -> Uninterned t -> t
  identity :: t -> Id t
  cache    :: Cache t

intern :: Interned t => Uninterned t -> t
intern bt = unsafeDupablePerformIO $ modifyMVar (getCache cache) go 
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

{-
type Var = Int

data Term
  = App {-# UNPACK #-} !(Id Term) !Term !Term
  | Lam {-# UNPACK #-} !(Id Term) {-# UNPACK #-} !Var !Term !Term
  | Pi  {-# UNPACK #-} !(Id Term) {-# UNPACK #-} !Var !Term !Term
  | Set {-# UNPACK #-} !(Id Term) {-# UNPACK #-} !Int
  deriving Show
data UninternedTerm 
  = BApp Term Term
  | BLam Var Term Term 
  | BPi  Var Term Term
  | BSet Int deriving Show
instance Interned Term where
  type Uninterned Term = UninternedTerm
  data Description Term = DApp (Id Term) (Id Term)
                 | DLam Var (Id Term) (Id Term)
                 | DPi  Var (Id Term) (Id Term)
                 | DSet Int deriving Show
  describe (BApp f a)   = DApp (identity f) (identity a) 
  describe (BLam v t e) = DLam v (identity t) (identity e)
  describe (BPi v t e)  = DPi v (identity t) (identity e)
  describe (BSet n) = DSet n
  identify i = go where
    go (BApp f a) = App i f a 
    go (BLam v t e) = Lam i v t e
    go (BPi v t e) = Pi i v t e
    go (BSet n) = Set i n
  identity (App i _ _) = i
  identity (Lam i _ _ _) = i
  identity (Pi i _ _ _) = i
  identity (Set i _) = i
  unintern (App _ f a) = BApp f a
  unintern (Lam _ v t e) = BLam v t e
  unintern (Pi _ v t e) = BPi v t e
  unintern (Set _ n) = BSet n
  cache = termCache

termCache :: Cache Term
termCache = mkCache
{-# NOINLINE termCache #-}

instance Eq (Description Term) where
  DApp f a     == DApp f' a'    = f == f' && a == a'
  DLam v t e   == DLam v' t' e' = v == v' && t == t' && e == e'
  DPi v t e    == DPi v' t' e'  = v == v' && t == t' && e == e'
  DSet n       == DSet n'       = n == n'
  _            == _             = False

instance Hashable (Description Term) where
  hash (DApp f a)   = 0 `hashWithSalt` f `hashWithSalt` a
  hash (DLam v t e) = 1 `hashWithSalt` v `hashWithSalt` t `hashWithSalt` e
  hash (DPi v t e)  = 2 `hashWithSalt` v `hashWithSalt` t `hashWithSalt` e
  hash (DSet n)     = 3 `hashWithSalt` n

instance Eq Term where
  (==) = (==) `on` identity

instance Ord Term where
  compare = compare `on` identity

app :: Term -> Term -> Term
app a b = intern (BApp a b)

lam :: Var -> Term -> Term -> Term
lam v t e = intern (BLam v t e)

pi :: Var -> Term -> Term -> Term
pi v t e = intern (BPi v t e)

set :: Int -> Term
set i = intern (BSet i)
-}
