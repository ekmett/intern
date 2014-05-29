module Data.Interned
  ( Interned(..)
  , Uninternable(..)
  , mkCache
  , Cache
  , cacheSize
  , Id
  , intern
  , eqId
  , compareId
  , touch
  ) where

import Data.Interned.Internal
