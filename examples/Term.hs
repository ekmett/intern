{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , DeriveGeneric
           , GeneralizedNewtypeDeriving #-}

module Term where

import Data.Function (on)
import Data.Hashable
import Data.Interned
import GHC.Generics

type Var = Int


data Term
  = App {-# UNPACK #-} !Id !Term !Term
  | Lam {-# UNPACK #-} !Id {-# UNPACK #-} !Var !Term !Term
  | Pi  {-# UNPACK #-} !Id {-# UNPACK #-} !Var !Term !Term
  | Set {-# UNPACK #-} !Id {-# UNPACK #-} !Int
  deriving Show

identity :: Term -> Int
identity (App i _ _) = i
identity (Lam i _ _ _) = i
identity (Pi i _ _ _) = i
identity (Set i _) = i

data UninternedTerm
  = BApp Term Term
  | BLam Var Term Term
  | BPi  Var Term Term
  | BSet Int deriving Show
instance Interned Term where
  type Uninterned Term = UninternedTerm
  data Description Term = DApp Id Id
                 | DLam Var Id Id
                 | DPi  Var Id Id
                 | DSet Int deriving (Show,Generic)
  describe (BApp f a)   = DApp (identity f) (identity a)
  describe (BLam v t e) = DLam v (identity t) (identity e)
  describe (BPi v t e)  = DPi v (identity t) (identity e)
  describe (BSet n) = DSet n
  identify i = go where
    go (BApp f a) = App i f a
    go (BLam v t e) = Lam i v t e
    go (BPi v t e) = Pi i v t e
    go (BSet n) = Set i n
  cache = termCache

instance Uninternable Term where
  unintern (App _ f a) = BApp f a
  unintern (Lam _ v t e) = BLam v t e
  unintern (Pi _ v t e) = BPi v t e
  unintern (Set _ n) = BSet n

termCache :: Cache Term
termCache = mkCache
{-# NOINLINE termCache #-}

instance Eq (Description Term) where
  DApp f a     == DApp f' a'    = f == f' && a == a'
  DLam v t e   == DLam v' t' e' = v == v' && t == t' && e == e'
  DPi v t e    == DPi v' t' e'  = v == v' && t == t' && e == e'
  DSet n       == DSet n'       = n == n'
  _            == _             = False

instance Hashable (Description Term)

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
