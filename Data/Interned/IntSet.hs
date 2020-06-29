{-# LANGUAGE MagicHash, TypeFamilies, FlexibleInstances, BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Interned.IntSet
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Edward Kmett 2011
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (TypeFamilies, MagicHash)
--
-- An efficient implementation of integer sets.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.IntSet (IntSet)
-- >  import qualified Data.IntSet as IntSet
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced set implementation (see "Data.Set").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseer.ist.psu.edu/okasaki98fast.html>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--      October 1968, pages 514-534.
--
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
--
-- Unlike the reference implementation in Data.IntSet, Data.Interned.IntSet
-- uses hash consing to ensure that there is only ever one copy of any given
-- IntSet in memory. This is enabled by the normal form of the PATRICIA trie.
--
-- This can mean a drastic reduction in the memory footprint of a program
-- in exchange for much more costly set manipulation.
--
-----------------------------------------------------------------------------

module Data.Interned.IntSet  (
            -- * Set type
              IntSet          -- instance Eq,Show

            -- * Operators
            , (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , isSubsetOf
            , isProperSubsetOf

            -- * Construction
            , empty
            , singleton
            , insert
            , delete

            -- * Combine
            , union, unions
            , difference
            , intersection

            -- * Filter
            , filter
            , partition
            , split
            , splitMember

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Map
            , map

            -- * Fold
            , fold

            -- * Conversion
            -- ** List
            , elems
            , toList
            , fromList

            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList

            -- * Debugging
            , showTree
            , showTreeWith
            ) where

import Prelude hiding (lookup,filter,foldr,foldl,null,map)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Interned.Internal
import Data.Bits
import Data.Hashable
import Text.Read
import GHC.Exts ( Word(..), Int(..), shiftRL# )

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif

#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- import Data.Typeable
-- import Data.Data (Data(..), mkNoRepType)

infixl 9 \\{-This comment teaches CPP correct behaviour -}

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Int -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Int
intFromNat w = fromIntegral w

shiftRL :: Nat -> Int -> Nat
shiftRL (W# x) (I# i) = W# (shiftRL# x i)

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
-- | /O(n+m)/. See 'difference'.
(\\) :: IntSet -> IntSet -> IntSet
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}
-- | A set of integers.
data IntSet
  = Nil
  | Tip {-# UNPACK #-} !Id {-# UNPACK #-} !Int
  | Bin {-# UNPACK #-} !Id {-# UNPACK #-} !Int {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !IntSet !IntSet
-- Invariant: Nil is never found as a child of Bin.
-- Invariant: The Mask is a power of 2.  It is the largest bit position at which
--            two elements of the set differ.
-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
-- Invariant: In Bin prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.

data UninternedIntSet
  = UNil
  | UTip !Int
  | UBin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !IntSet !IntSet


tip :: Int -> IntSet
tip n = intern (UTip n)

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r = intern (UBin p m l r)

bin_ :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
bin_ p m l r = intern (UBin p m l r)

identity :: IntSet -> Id
identity Nil = 0
identity (Tip i _) = i
identity (Bin i _ _ _ _ _) = i

instance Interned IntSet where
  type Uninterned IntSet = UninternedIntSet
  data Description IntSet
    = DNil
    | DTip {-# UNPACK #-} !Int
    | DBin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask {-# UNPACK #-} !Id {-# UNPACK #-} !Id
    deriving Eq
  describe UNil = DNil
  describe (UTip j) = DTip j
  describe (UBin p m l r) = DBin p m (identity l) (identity r)
  cacheWidth _ = 16384 -- a huge cache width!
  seedIdentity _ = 1
  identify _ UNil = Nil
  identify i (UTip j) = Tip i j
  identify i (UBin p m l r) = Bin i (size l + size r) p m l r
  cache = intSetCache

instance Hashable (Description IntSet) where
  hashWithSalt s DNil = s `hashWithSalt` (0 :: Int)
  hashWithSalt s (DTip n) = s `hashWithSalt` (1 :: Int) `hashWithSalt` n
  hashWithSalt s (DBin p m l r) = s `hashWithSalt` (2 :: Int) `hashWithSalt` p `hashWithSalt` m `hashWithSalt` l `hashWithSalt` r

intSetCache :: Cache IntSet
intSetCache = mkCache
{-# NOINLINE intSetCache #-}

instance Uninternable IntSet where
  unintern Nil = UNil
  unintern (Tip _ j) = UTip j
  unintern (Bin _ _ p m l r) = UBin p m l r

type Prefix = Int
type Mask   = Int

#if MIN_VERSION_base(4,9,0)
instance Semigroup IntSet where
    (<>) = union
#endif

instance Monoid IntSet where
    mempty  = empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = union
#endif
    mconcat = unions


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the set empty?
null :: IntSet -> Bool
null Nil = True
null _   = False

-- | /O(1)/. Cardinality of the set.
size :: IntSet -> Int
size t
  = case t of
      Bin _ s _ _ _ _ -> s
      Tip _ _ -> 1
      Nil   -> 0


-- | /O(min(n,W))/. Is the value a member of the set?
member :: Int -> IntSet -> Bool
member x t
  = case t of
      Bin _ _ p m l r
        | nomatch x p m -> False
        | zero x m      -> member x l
        | otherwise     -> member x r
      Tip _ y -> (x==y)
      Nil     -> False

-- | /O(min(n,W))/. Is the element not in the set?
notMember :: Int -> IntSet -> Bool
notMember k = not . member k

-- 'lookup' is used by 'intersection' for left-biasing
lookup :: Int -> IntSet -> Maybe Int
lookup k t
  = let nk = natFromInt k  in seq nk (lookupN nk t)

lookupN :: Nat -> IntSet -> Maybe Int
lookupN k t
  = case t of
      Bin _ _ _ m l r
        | zeroN k (natFromInt m) -> lookupN k l
        | otherwise              -> lookupN k r
      Tip _ kx
        | (k == natFromInt kx)  -> Just kx
        | otherwise             -> Nothing
      Nil -> Nothing


{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty set.
empty :: IntSet
empty = Nil

-- | /O(1)/. A set of one element.
singleton :: Int -> IntSet
singleton x = tip x



{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Add a value to the set. When the value is already
-- an element of the set, it is replaced by the new one, ie. 'insert'
-- is left-biased.
insert :: Int -> IntSet -> IntSet
insert x t
  = case t of
      Bin _ _ p m l r
        | nomatch x p m -> join x (tip x) p t
        | zero x m      -> bin_ p m (insert x l) r
        | otherwise     -> bin_ p m l (insert x r)
      Tip _ y
        | x==y          -> tip x
        | otherwise     -> join x (tip x) y t
      Nil -> tip x

-- right-biased insertion, used by 'union'
insertR :: Int -> IntSet -> IntSet
insertR x t
  = case t of
      Bin _ _ p m l r
        | nomatch x p m -> join x (tip x) p t
        | zero x m      -> bin_ p m (insert x l) r
        | otherwise     -> bin_ p m l (insert x r)
      Tip _ y
        | x==y          -> t
        | otherwise     -> join x (tip x) y t
      Nil -> tip x

-- | /O(min(n,W))/. Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: Int -> IntSet -> IntSet
delete x t
  = case t of
      Bin _ _ p m l r
        | nomatch x p m -> t
        | zero x m      -> bin p m (delete x l) r
        | otherwise     -> bin p m l (delete x r)
      Tip _ y
        | x==y          -> Nil
        | otherwise     -> t
      Nil -> Nil


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of sets.
unions :: [IntSet] -> IntSet
unions xs = foldlStrict union empty xs


-- | /O(n+m)/. The union of two sets.
union :: IntSet -> IntSet -> IntSet
union t1@(Bin _ _ p1 m1 l1 r1) t2@(Bin _ _ p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = bin_ p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = bin_ p1 m1 (union l1 t2) r1
            | otherwise         = bin_ p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = bin_ p2 m2 (union t1 l2) r2
            | otherwise         = bin_ p2 m2 l2 (union t1 r2)

union (Tip _ x) t = insert x t
union t (Tip _ x) = insertR x t  -- right bias
union Nil t       = t
union t Nil       = t


{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two sets.
difference :: IntSet -> IntSet -> IntSet
difference t1@(Bin _ _ p1 m1 l1 r1) t2@(Bin _ _ p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (difference l1 t2) r1
                | otherwise         = bin p1 m1 l1 (difference r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = difference t1 l2
                | otherwise         = difference t1 r2

difference t1@(Tip _ x) t2
  | member x t2  = Nil
  | otherwise    = t1

difference Nil _       = Nil
difference t (Tip _ x) = delete x t
difference t Nil       = t



{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The intersection of two sets.
intersection :: IntSet -> IntSet -> IntSet
intersection t1@(Bin _ _ p1 m1 l1 r1) t2@(Bin _ _ p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersection l1 l2) (intersection r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersection l1 t2
                  | otherwise         = intersection r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersection t1 l2
                  | otherwise         = intersection t1 r2

intersection t1@(Tip _ x) t2
  | member x t2  = t1
  | otherwise    = Nil
intersection t (Tip _ x)
  = case lookup x t of
      Just y  -> tip y
      Nothing -> Nil
intersection Nil _ = Nil
intersection _ Nil = Nil


{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: IntSet -> IntSet -> Bool
isProperSubsetOf t1 t2
  = case subsetCmp t1 t2 of
      LT -> True
      _  -> False

subsetCmp :: IntSet -> IntSet -> Ordering
subsetCmp t1@(Bin _ _ p1 m1 l1 r1) (Bin _ _ p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = case subsetCmpLt of
                       GT -> GT
                       _ -> LT
  | p1 == p2       = subsetCmpEq
  | otherwise      = GT  -- disjoint
  where
    subsetCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = subsetCmp t1 l2
                | otherwise         = subsetCmp t1 r2
    subsetCmpEq = case (subsetCmp l1 l2, subsetCmp r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

subsetCmp (Bin _ _ _ _ _ _) _  = GT
subsetCmp (Tip _ x) (Tip _ y)
  | x==y       = EQ
  | otherwise  = GT  -- disjoint
subsetCmp (Tip _ x) t
  | member x t = LT
  | otherwise  = GT  -- disjoint
subsetCmp Nil Nil = EQ
subsetCmp Nil _   = LT

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.

isSubsetOf :: IntSet -> IntSet -> Bool
isSubsetOf t1@(Bin _ _ p1 m1 l1 r1) (Bin _ _ p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubsetOf t1 l2
                                                      else isSubsetOf t1 r2)
  | otherwise      = (p1==p2) && isSubsetOf l1 l2 && isSubsetOf r1 r2
isSubsetOf (Bin _ _ _ _ _ _) _  = False
isSubsetOf (Tip _ x) t          = member x t
isSubsetOf Nil _                = True


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (Int -> Bool) -> IntSet -> IntSet
filter predicate t
  = case t of
      Bin _ _ p m l r
        -> bin p m (filter predicate l) (filter predicate r)
      Tip _ x
        | predicate x -> t
        | otherwise   -> Nil
      Nil -> Nil

-- | /O(n)/. partition the set according to some predicate.
partition :: (Int -> Bool) -> IntSet -> (IntSet,IntSet)
partition predicate t
  = case t of
      Bin _ _ p m l r
        -> let (l1,l2) = partition predicate l
               (r1,r2) = partition predicate r
           in (bin p m l1 r1, bin p m l2 r2)
      Tip _ x
        | predicate x -> (t,Nil)
        | otherwise   -> (Nil,t)
      Nil -> (Nil,Nil)


-- | /O(min(n,W))/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: Int -> IntSet -> (IntSet,IntSet)
split x t
  = case t of
      Bin _ _ _ m l r
        | m < 0       -> if x >= 0 then let (lt,gt) = split' x l in (union r lt, gt)
                                   else let (lt,gt) = split' x r in (lt, union gt l)
                                   -- handle negative numbers.
        | otherwise   -> split' x t
      Tip _ y
        | x>y         -> (t,Nil)
        | x<y         -> (Nil,t)
        | otherwise   -> (Nil,Nil)
      Nil             -> (Nil, Nil)

split' :: Int -> IntSet -> (IntSet,IntSet)
split' x t
  = case t of
      Bin _ _ p m l r
        | match x p m -> if zero x m then let (lt,gt) = split' x l in (lt,union gt r)
                                     else let (lt,gt) = split' x r in (union l lt,gt)
        | otherwise   -> if x < p then (Nil, t)
                                  else (t, Nil)
      Tip _ y
        | x>y       -> (t,Nil)
        | x<y       -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Int -> IntSet -> (IntSet,Bool,IntSet)
splitMember x t
  = case t of
      Bin _ _ _ m l r
        | m < 0       -> if x >= 0 then let (lt,found,gt) = splitMember' x l in (union r lt, found, gt)
                                   else let (lt,found,gt) = splitMember' x r in (lt, found, union gt l)
                                   -- handle negative numbers.
        | otherwise   -> splitMember' x t
      Tip _ y
        | x>y       -> (t,False,Nil)
        | x<y       -> (Nil,False,t)
        | otherwise -> (Nil,True,Nil)
      Nil -> (Nil,False,Nil)

splitMember' :: Int -> IntSet -> (IntSet,Bool,IntSet)
splitMember' x t
  = case t of
      Bin _ _ p m l r
         | match x p m ->  if zero x m then let (lt,found,gt) = splitMember x l in (lt,found,union gt r)
                                       else let (lt,found,gt) = splitMember x r in (union l lt,found,gt)
         | otherwise   -> if x < p then (Nil, False, t)
                                   else (t, False, Nil)
      Tip _ y
        | x>y       -> (t,False,Nil)
        | x<y       -> (Nil,False,t)
        | otherwise -> (Nil,True,Nil)
      Nil -> (Nil,False,Nil)



{----------------------------------------------------------------------
  Min/Max
----------------------------------------------------------------------}

-- | /O(min(n,W))/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: IntSet -> Maybe (Int, IntSet)
maxView t
    = case t of
        Bin _ _ p m l r | m < 0 -> let (result,t') = maxViewUnsigned l in Just (result, bin p m t' r)
        Bin _ _ p m l r         -> let (result,t') = maxViewUnsigned r in Just (result, bin p m l t')
        Tip _ y -> Just (y,Nil)
        Nil -> Nothing

maxViewUnsigned :: IntSet -> (Int, IntSet)
maxViewUnsigned t
    = case t of
        Bin _ _ p m l r -> let (result,t') = maxViewUnsigned r in (result, bin p m l t')
        Tip _ y -> (y, Nil)
        Nil -> error "maxViewUnsigned Nil"

-- | /O(min(n,W))/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: IntSet -> Maybe (Int, IntSet)
minView t
    = case t of
        Bin _ _ p m l r | m < 0 -> let (result,t') = minViewUnsigned r in Just (result, bin p m l t')
        Bin _ _ p m l r         -> let (result,t') = minViewUnsigned l in Just (result, bin p m t' r)
        Tip _ y -> Just (y, Nil)
        Nil -> Nothing

minViewUnsigned :: IntSet -> (Int, IntSet)
minViewUnsigned t
    = case t of
        Bin _ _ p m l r -> let (result,t') = minViewUnsigned l in (result, bin p m t' r)
        Tip _ y -> (y, Nil)
        Nil -> error "minViewUnsigned Nil"

-- | /O(min(n,W))/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: IntSet -> (Int, IntSet)
deleteFindMin = fromMaybe (error "deleteFindMin: empty set has no minimal element") . minView

-- | /O(min(n,W))/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: IntSet -> (Int, IntSet)
deleteFindMax = fromMaybe (error "deleteFindMax: empty set has no maximal element") . maxView


-- | /O(min(n,W))/. The minimal element of the set.
findMin :: IntSet -> Int
findMin Nil = error "findMin: empty set has no minimal element"
findMin (Tip _ x) = x
findMin (Bin _ _ _ m l r)
  |   m < 0   = find r
  | otherwise = find l
    where find (Tip _ x)          = x
          find (Bin _ _ _ _ l' _) = find l'
          find Nil                = error "findMin Nil"

-- | /O(min(n,W))/. The maximal element of a set.
findMax :: IntSet -> Int
findMax Nil = error "findMax: empty set has no maximal element"
findMax (Tip _ x) = x
findMax (Bin _ _ _ m l r)
  |   m < 0   = find l
  | otherwise = find r
    where find (Tip _ x)          = x
          find (Bin _ _ _ _ _ r') = find r'
          find Nil                = error "findMax Nil"


-- | /O(min(n,W))/. Delete the minimal element.
deleteMin :: IntSet -> IntSet
deleteMin = maybe (error "deleteMin: empty set has no minimal element") snd . minView

-- | /O(min(n,W))/. Delete the maximal element.
deleteMax :: IntSet -> IntSet
deleteMax = maybe (error "deleteMax: empty set has no maximal element") snd . maxView

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | /O(n*min(n,W))/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: (Int->Int) -> IntSet -> IntSet
map f = fromList . List.map f . toList

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over the elements of a set in an unspecified order.
--
-- > sum set   == fold (+) 0 set
-- > elems set == fold (:) [] set
fold :: (Int -> b -> b) -> b -> IntSet -> b
fold f z t
  = case t of
      Bin _ _ 0 m l r | m < 0 -> foldr f (foldr f z l) r
      -- put negative numbers before.
      Bin _ _ _ _ _ _ -> foldr f z t
      Tip _ x         -> f x z
      Nil             -> z

foldr :: (Int -> b -> b) -> b -> IntSet -> b
foldr f z t
  = case t of
      Bin _ _ _ _ l r -> foldr f (foldr f z r) l
      Tip _ x         -> f x z
      Nil             -> z

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. The elements of a set. (For sets, this is equivalent to toList)
elems :: IntSet -> [Int]
elems s = toList s

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Convert the set to a list of elements.
toList :: IntSet -> [Int]
toList t = fold (:) [] t

-- | /O(n)/. Convert the set to an ascending list of elements.
toAscList :: IntSet -> [Int]
toAscList t = toList t

-- | /O(n*min(n,W))/. Create a set from a list of integers.
fromList :: [Int] -> IntSet
fromList xs = foldlStrict ins empty xs
  where
    ins t x  = insert x t

-- | /O(n)/. Build a set from an ascending list of elements.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: [Int] -> IntSet
fromAscList [] = Nil
fromAscList (x0 : xs0) = fromDistinctAscList (combineEq x0 xs0)
  where
    combineEq x' [] = [x']
    combineEq x' (x:xs)
      | x==x'     = combineEq x' xs
      | otherwise = x' : combineEq x xs

-- | /O(n)/. Build a set from an ascending list of distinct elements.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [Int] -> IntSet
fromDistinctAscList []         = Nil
fromDistinctAscList (z0 : zs0) = work z0 zs0 Nada
  where
    work x []     stk = finish x (tip x) stk
    work x (z:zs) stk = reduce z zs (branchMask z x) x (tip x) stk

    reduce z zs _ px tx Nada = work z zs (Push px tx Nada)
    reduce z zs m px tx stk@(Push py ty stk') =
        let mxy = branchMask px py
            pxy = mask px mxy
        in  if shorter m mxy
                 then reduce z zs m pxy (bin_ pxy mxy ty tx) stk'
                 else work z zs (Push px tx stk)

    finish _  t  Nada = t
    finish px tx (Push py ty stk) = finish p (join py ty px tx) stk
        where m = branchMask px py
              p = mask px m

data Stack = Push {-# UNPACK #-} !Prefix !IntSet !Stack | Nada

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: IntSet -> String
showTree s
  = showTreeWith True False s

{- | /O(n)/. The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the set. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Bool -> Bool -> IntSet -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Bool -> [String] -> [String] -> IntSet -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin _ _ p m l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBin p m) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip _ x
          -> showsBars lbars . showString " " . shows x . showString "\n"
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Bool -> [String] -> IntSet -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin _ _ p m l r
          -> showsBars bars . showString (showBin p m) . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip _ x
          -> showsBars bars . showString " " . shows x . showString "\n"
      Nil -> showsBars bars . showString "|\n"

showBin :: Prefix -> Mask -> String
showBin _ _
  = "*" -- ++ show (p,m)

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}

-- /O(1)/
instance Eq IntSet where
  Nil             == Nil             = True
  Tip i _         == Tip j _         = i == j
  Bin i _ _ _ _ _ == Bin j _ _ _ _ _ = i == j
  _ == _ = False

{--------------------------------------------------------------------
  Ord
  NB: this ordering is not the ordering implied by the elements
      but is usable for comparison
--------------------------------------------------------------------}
instance Ord IntSet where
  Nil `compare` Nil = EQ
  Nil `compare` Tip _ _ = LT
  Nil `compare` Bin _ _ _ _ _ _ = LT
  Tip _ _ `compare` Nil = GT
  Tip i _ `compare` Tip j _ = compare i j
  Tip i _ `compare` Bin j _ _ _ _ _ = compare i j
  Bin _ _ _ _ _ _ `compare` Nil = GT
  Bin i _ _ _ _ _ `compare` Tip j _ = compare i j
  Bin i _ _ _ _ _ `compare` Bin j _ _ _ _ _ = compare i j
  -- compare s1 s2 = compare (toAscList s1) (toAscList s2)

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show IntSet where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

{--------------------------------------------------------------------
  Hashable
--------------------------------------------------------------------}
instance Hashable IntSet where
  hashWithSalt s x = hashWithSalt s $ identity x

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance Read IntSet where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault


{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Prefix -> IntSet -> Prefix -> IntSet -> IntSet
join p1 t1 p2 t2
  | zero p1 m = bin_ p m t1 t2
  | otherwise = bin_ p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m



{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Int -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

nomatch,match :: Int -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p

match i p m
  = (mask i m) == p

-- Suppose a is largest such that 2^a divides 2*m.
-- Then mask i m is i with the low a bits zeroed out.
mask :: Int -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)

zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))

{----------------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently in
  three ways:
  * convert to a floating point value and the mantissa tells us the
    [log2(x)] that corresponds with the highest bit position. The mantissa
    is retrieved either via the standard C function [frexp] or by some bit
    twiddling on IEEE compatible numbers (float). Note that one needs to
    use at least [double] precision for an accurate mantissa of 32 bit
    numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an AMD
  Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even faster
  than a single CISC instruction (BSR)!
----------------------------------------------------------------------}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the
  highest bit and than taking an exclusive or with the original value.
  Although the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)


