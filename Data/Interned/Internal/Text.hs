{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.Interned.Internal.Text
  ( InternedText(..)
  ) where

import Data.String
import Data.Interned
import Data.Text
import Data.Hashable
import Data.Function (on)

data InternedText = InternedText 
  {-# UNPACK #-} !Id
  {-# UNPACK #-} !Text

instance IsString InternedText where
  fromString = intern . pack

instance Eq InternedText where
  (==) = (==) `on` identity

instance Ord InternedText where
  compare = compare `on` identity

instance Show InternedText where
  showsPrec d (InternedText _ b) = showsPrec d b

instance Interned InternedText where
  type Uninterned InternedText = Text
  data Description InternedText = DT {-# UNPACK #-} !Text deriving (Eq) 
  describe = DT
  identify = InternedText
  identity (InternedText i _) = i
  cache = itCache

instance Uninternable InternedText where
  unintern (InternedText _ b) = b 

instance Hashable (Description InternedText) where
  hash (DT h) = hash h

itCache :: Cache InternedText
itCache = mkCache
{-# NOINLINE itCache #-}
