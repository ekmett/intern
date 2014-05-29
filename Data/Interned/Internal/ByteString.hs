{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Data.Interned.Internal.ByteString
  ( InternedByteString(..)
  ) where

import Data.String
import Data.Interned
import Data.ByteString
import Data.ByteString.Char8 as Char8
import Data.Hashable

data InternedByteString = InternedByteString
  { internedByteStringId :: {-# UNPACK #-} !Id
  , uninternByteString   :: {-# UNPACK #-} !ByteString
  }

instance IsString InternedByteString where
  fromString = intern . Char8.pack

instance Eq InternedByteString where
  (==) = eqId

instance Ord InternedByteString where
  compare = compareId

instance Show InternedByteString where
  showsPrec d (InternedByteString _ b) = showsPrec d b

instance Interned InternedByteString where
  type Uninterned InternedByteString = ByteString
  newtype Description InternedByteString = DBS ByteString deriving (Eq,Hashable)
  describe = DBS
  identify = InternedByteString
  identity = internedByteStringId
  cache = ibsCache

instance Uninternable InternedByteString where
  unintern = uninternByteString

ibsCache :: Cache InternedByteString
ibsCache = mkCache
{-# NOINLINE ibsCache #-}
