module Data.Interned
  ( Interned(..)
  , Uninternable(..)
  , mkCache
  , Cache
  , cacheSize
  , Id(..)
  , intern
  ) where

import Data.Interned.Internal
