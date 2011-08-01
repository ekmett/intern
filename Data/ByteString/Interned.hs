{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.ByteString.Interned
  ( InternedByteString(..)
  ) where

import Data.String
import Data.Interned
import Data.ByteString
import Data.ByteString.Char8 as Char8
import Data.Hashable
import Data.Function (on)

data InternedByteString = InternedByteString 
  {-# UNPACK #-} !(Id InternedByteString)
  {-# UNPACK #-} !ByteString

instance IsString InternedByteString where
  fromString = intern . Char8.pack

instance Eq InternedByteString where
  (==) = (==) `on` identity

instance Ord InternedByteString where
  compare = compare `on` identity

instance Show InternedByteString where
  showsPrec d (InternedByteString _ b) = showsPrec d b

instance Interned InternedByteString where
  type Uninterned InternedByteString = ByteString
  data Description InternedByteString = DBS {-# UNPACK #-} !ByteString
    deriving (Eq) 
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
