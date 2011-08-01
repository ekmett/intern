{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.String.Interned
  ( InternedString
  ) where

import Data.String
import Data.Interned
import Data.Hashable
import Data.Foldable
import Data.Function (on)

data InternedString = IS 
  {-# UNPACK #-} !(Id InternedString)
  String

instance IsString InternedString where
  fromString = intern

instance Eq InternedString where
  (==) = (==) `on` identity

instance Ord InternedString where
  compare = compare `on` identity

instance Show InternedString where
  showsPrec d (IS _ b) = showsPrec d b

instance Interned InternedString where
  type Uninterned InternedString = String
  data Description InternedString = Cons {-# UNPACK #-} !Char String | Nil
    deriving (Eq) 
  describe (c:cs) = Cons c cs
  describe []     = Nil
  identify = IS
  identity (IS i _) = i
  cache = stringCache

instance Uninternable InternedString where
  unintern (IS _ b) = b 

instance Hashable (Description InternedString) where
  hash (Cons c s) = foldl' hashWithSalt (hashWithSalt 0 c) s
  hash Nil        = 0

stringCache :: Cache InternedString
stringCache = mkCache
{-# NOINLINE stringCache #-}
