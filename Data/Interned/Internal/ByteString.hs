{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Data.Interned.Internal.ByteString
  ( InternedByteString(..)
  ) where

import Data.String
import Data.Interned
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Hashable

data InternedByteString = InternedByteString
  { internedByteStringId :: {-# UNPACK #-} !Id
  , uninternByteString   :: {-# UNPACK #-} !ByteString
  }

instance IsString InternedByteString where
  fromString = intern . Char8.pack

instance Eq InternedByteString where
  InternedByteString i _ == InternedByteString j _ = i == j

instance Ord InternedByteString where
  InternedByteString i _ `compare` InternedByteString j _ = i `compare` j

instance Show InternedByteString where
  showsPrec d (InternedByteString _ b) = showsPrec d b

instance Hashable InternedByteString where
  hashWithSalt s (InternedByteString i _) = hashWithSalt s i

instance Interned InternedByteString where
  type Uninterned InternedByteString = ByteString
  newtype Description InternedByteString = DBS ByteString deriving (Eq,Hashable)
  describe = DBS
  identify = InternedByteString
  cache = ibsCache

instance Uninternable InternedByteString where
  unintern = uninternByteString

ibsCache :: Cache InternedByteString
ibsCache = mkCache
{-# NOINLINE ibsCache #-}
