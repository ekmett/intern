{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.Interned.Internal.Text
  ( InternedText(..)
  ) where

import Data.String
import Data.Interned
import Data.Text
import Data.Hashable

data InternedText = InternedText
  { internedTextId :: {-# UNPACK #-} !Id
  , uninternedText :: {-# UNPACK #-} !Text
  }

instance IsString InternedText where
  fromString = intern . pack

instance Eq InternedText where
  InternedText i _ == InternedText j _ = i == j

instance Ord InternedText where
  compare (InternedText i _) (InternedText j _) = compare i j

instance Show InternedText where
  showsPrec d (InternedText _ b) = showsPrec d b

instance Hashable InternedText where
  hashWithSalt s (InternedText i _) = hashWithSalt s i

instance Interned InternedText where
  type Uninterned InternedText = Text
  newtype Description InternedText = DT Text deriving (Eq)
  describe = DT
  identify = InternedText
  cache = itCache

instance Uninternable InternedText where
  unintern (InternedText _ b) = b

instance Hashable (Description InternedText) where
  hashWithSalt s (DT h) = hashWithSalt s h

itCache :: Cache InternedText
itCache = mkCache
{-# NOINLINE itCache #-}
