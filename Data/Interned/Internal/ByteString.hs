{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.Interned.Internal.ByteString
  ( InternedByteString(..)
  ) where

import Data.String
import Data.Interned
import Data.ByteString
import Data.ByteString.Char8 as Char8
import Data.Hashable

data InternedByteString = InternedByteString 
  {-# UNPACK #-} !Id
  {-# UNPACK #-} !ByteString

instance IsString InternedByteString where
  fromString = intern . Char8.pack

instance Eq InternedByteString where
  InternedByteString i _ == InternedByteString j _ = i == j

instance Ord InternedByteString where
  InternedByteString i _ `compare` InternedByteString j _ = i `compare` j

instance Show InternedByteString where
  showsPrec d (InternedByteString _ b) = showsPrec d b

instance Interned InternedByteString where
  type Uninterned InternedByteString = ByteString
  data Description InternedByteString = DBS {-# UNPACK #-} !ByteString deriving (Eq) 
  describe = DBS
  identify = InternedByteString
  identity (InternedByteString i _) = i
  cache = ibsCache

instance Uninternable InternedByteString where
  unintern (InternedByteString _ b) = b 

instance Hashable (Description InternedByteString) where
  hash (DBS h) = hash h

ibsCache :: Cache InternedByteString
ibsCache = mkCache
{-# NOINLINE ibsCache #-}
