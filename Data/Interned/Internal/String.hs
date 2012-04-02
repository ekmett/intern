{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.Interned.Internal.String
  ( InternedString(..)
  ) where

import Data.String
import Data.Interned
import Data.Hashable
import Data.Foldable

data InternedString = IS
  { internedStringId :: {-# UNPACK #-} !Id
  , uninternString :: String
  }

instance IsString InternedString where
  fromString = intern

instance Eq InternedString where
  IS i _ == IS j _ = i == j

instance Ord InternedString where
  compare (IS i _) (IS j _) = compare i j

instance Show InternedString where
  showsPrec d (IS _ b) = showsPrec d b

instance Interned InternedString where
  type Uninterned InternedString = String
  data Description InternedString = Cons {-# UNPACK #-} !Char String | Nil
    deriving (Eq)
  describe (c:cs) = Cons c cs
  describe []     = Nil
  identify = IS
  cache = stringCache

instance Uninternable InternedString where
  unintern = uninternString

instance Hashable (Description InternedString) where
  hash (Cons c s) = foldl' hashWithSalt (hashWithSalt 0 c) s
  hash Nil        = 0

stringCache :: Cache InternedString
stringCache = mkCache
{-# NOINLINE stringCache #-}
