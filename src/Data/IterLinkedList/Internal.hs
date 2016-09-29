{-# LANGUAGE TypeFamilies, NamedFieldPuns, RecordWildCards, FlexibleInstances #-}

{-|
Module      : Data.IterLinkedList.Internal
Description : A pure linked list which is mutable through iterators. (with exported internals)
Copyright   : (c) CindyLinz, 2016
License     : MIT
Maintainer  : cindylinz@gmail.com
Portability : portable

A pure linked list with is mutable through iterators.

Exported internals.
-}

module Data.IterLinkedList.Internal
  ( LinkedList(..)
  , IterLinkedList(..)
  , LinkedListContainer(..)
  , firstIter
  , lastIter
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

-- | The list
data LinkedList iter value = LinkedList
  { newKey :: iter -- ^ pre-allocated iterator value for the next inserted element
  , firstKey :: iter -- ^ iterator to the first element (equals to `newKey` when `null container`)
  , lastKey :: iter -- ^ iterator to the last element (equals to `newKey` when `null container`)
  , container :: LinkedListContainer iter value
  }

instance Show value => Show (LinkedList Int value) where
  show ls = "fromList " ++ show (toList ls)

instance Show value => Show (LinkedList Integer value) where
  show ls = "fromList " ++ show (toList ls)

-- | The internal container
type family LinkedListContainer iter value
type instance LinkedListContainer Int value = IM.IntMap (Int, value, Int)
type instance LinkedListContainer Integer value = M.Map Integer (Integer, value, Integer)

-- | Polymorphic operations on the list
class IterLinkedList iter where
  {-# MINIMAL null, get, (set | modify), next, prev, empty, singleton, insertBefore, insertAfter, delete, toList #-}

  -- | See if this list is an empty list. @O(1)@
  null :: LinkedList iter value -> Bool

  -- | Get the element value. @O(lg N)@
  get :: iter -> LinkedList iter value -> Maybe value

  -- | Get the element value. Get undefined if not found. @O(lg N)@
  get' :: iter -> LinkedList iter value -> value
  get' iter list = case get iter list of
    Just value -> value
    Nothing -> undefined

  -- | Set the element value.
  --   Return the original list if the iterator is not in the list @O(lg N)@
  set :: iter -> value -> LinkedList iter value -> LinkedList iter value
  set iter value list = modify iter (const value) list

  -- | Modify the element value.
  --   Return the original list if the iterator is not in the list @O(lg N)@
  modify :: iter -> (value -> value) -> LinkedList iter value -> LinkedList iter value
  modify iter f list = case get iter list of
    Just value -> set iter (f value) list
    Nothing -> list

  -- | Get the next iterator.
  --   If the specified iterator is the last one, or isn't in the list,
  --   return the original one. @O(lg N)@
  next :: LinkedList iter value -> iter -> iter

  -- | Get the previous iterator.
  --   If the specified iterator is the first one, or isn't in the list,
  --   return the original one. @O(lg N)@
  prev :: LinkedList iter value -> iter -> iter

  -- | Get an empty list. @O(1)@
  empty :: LinkedList iter value

  -- | Get a list with exactly one element. @O(1)@
  singleton :: value -> LinkedList iter value

  -- | Insert a new element before the specified iterator.
  --   If the list is empty, just insert the new element as the only element.
  --   If the specified iterator can't be found, prepend the new element to the whole list.
  --   @O(lg N)@
  insertBefore :: iter -> value -> LinkedList iter value -> LinkedList iter value

  -- | Insert a new element after the specified iterator.
  --   If the list is empty, just insert the new element as the only element.
  --   If the specified iterator can't be found, append the new element to the whole list.
  --   @O(lg N)@
  insertAfter :: iter -> value -> LinkedList iter value -> LinkedList iter value

  -- | Delete the specified element from the list.
  --   If there's no such element in the list, return the original list.
  --   @O(lg N)@
  delete :: iter -> LinkedList iter value -> LinkedList iter value

  -- | Get a LinkedList from a list
  --   @O(N)@
  fromList :: [value] -> LinkedList iter value
  fromList [] = empty
  fromList (a:as) = go (singleton a) as where
    go list (a:as) = go (insertAfter (lastIter list) a list) as
    go list _ = list

  -- | Get a list from a LinkedList
  --   @O(N lg N)@
  toList :: LinkedList iter value -> [value]

-- | Get the first iterator.
--   If the list is empty, you'll still get an unusable one.
--   You can't get the value from the unusable iterator.
--   @O(lg N)@
firstIter :: LinkedList iter value -> iter
firstIter LinkedList{firstKey} = firstKey

-- | Get the last iterator.
--   If the list is empty, you'll still get an unusable one.
--   You can't get the value from the unusable iterator.
--   @O(lg N)@
lastIter :: LinkedList iter value -> iter
lastIter LinkedList{lastKey} = lastKey

instance IterLinkedList Int where

  null (LinkedList {container = cntr}) = IM.null cntr

  get iter (LinkedList {..}) =
    fmap (\(prev, value, next) -> value) (IM.lookup iter container)

  get' iter (LinkedList {..}) = case IM.lookup iter container of
    Just (prev, value, next) -> value
    _ -> undefined

  set iter value list@(LinkedList {..}) = list{container = IM.adjust f iter container}
    where
      f (prevKey, _, nextKey) = (prevKey, value, nextKey)

  modify iter f list@(LinkedList {..}) = list{container = IM.adjust g iter container}
    where
      g (prevKey, value, nextKey) = (prevKey, f value, nextKey)

  next (LinkedList {..}) iter = case IM.lookup iter container of
    Just (prev, value, next) -> next
    _ -> iter

  prev (LinkedList {..}) iter = case IM.lookup iter container of
    Just (prev, value, next) -> prev
    _ -> iter

  empty = LinkedList
    { newKey = minBound
    , firstKey = minBound
    , lastKey = minBound
    , container = IM.empty
    }

  singleton value = LinkedList
    { newKey = minBound + 1
    , firstKey = minBound
    , lastKey = minBound
    , container = IM.singleton minBound (minBound, value, minBound)
    }

  insertBefore iter value (LinkedList {..}) = LinkedList
    { newKey = newKey + 1
    , firstKey = if isPrepend then newKey else firstKey
    , lastKey = lastKey
    , container
      = IM.insert newKey (prevKey, value, nextKey)
      $ IM.adjust
        (\(prevPrevKey, prevValue, prevNextKey) -> (prevPrevKey, prevValue, newKey))
        prevKey
      $ IM.adjust
        (\(nextPrevKey, nextValue, nextNextKey) -> (newKey, nextValue, nextNextKey))
        nextKey
      $ container
    }
    where
      (isPrepend, prevKey, nextKey) = case IM.lookup iter container of
        Nothing -> (True, newKey, firstKey)
        Just (iterPrevKey, iterValue, iterNextKey)
          | iterPrevKey == iter -> (True, newKey, iter)
          | otherwise -> (False, iterPrevKey, iter)

  insertAfter iter value (LinkedList {..}) = LinkedList
    { newKey = newKey + 1
    , firstKey = firstKey
    , lastKey = if isAppend then newKey else lastKey
    , container
      = IM.insert newKey (prevKey, value, nextKey)
      $ IM.adjust
        (\(prevPrevKey, prevValue, prevNextKey) -> (prevPrevKey, prevValue, newKey))
        prevKey
      $ IM.adjust
        (\(nextPrevKey, nextValue, nextNextKey) -> (newKey, nextValue, nextNextKey))
        nextKey
      $ container
    }
    where
      (isAppend, prevKey, nextKey) = case IM.lookup iter container of
        Nothing -> (True, lastKey, newKey)
        Just (iterPrevKey, iterValue, iterNextKey)
          | iterNextKey == iter -> (True, iter, newKey)
          | otherwise -> (False, iter, iterNextKey)

  delete iter list@(LinkedList {..}) = case IM.lookup iter container of
    Nothing -> list
    Just (iterPrevKey, iterValue, iterNextKey) -> LinkedList
      { newKey = newKey
      , firstKey = if firstKey == iter then iterNextKey else firstKey
      , lastKey = if lastKey == iter then iterPrevKey else lastKey
      , container
        = IM.adjust (\(prevPrevKey, prevValue, prevNextKey) ->
            (prevPrevKey, prevValue, if iterNextKey == iter then iterPrevKey else iterNextKey)
          ) iterPrevKey
        $ IM.adjust (\(nextPrevKey, nextValue, nextNextKey) ->
            (if iterPrevKey == iter then iterNextKey else iterPrevKey, nextValue, nextNextKey)
          ) iterNextKey
        $ IM.delete iter container
      }

  fromList [] = empty
  fromList as = LinkedList
    { newKey = minBound + len
    , firstKey = minBound
    , lastKey = lastKey
    , container = IM.fromList $ zip [minBound..] $ zip3 (minBound : [minBound..]) as ([minBound + 1 .. lastKey] ++ [lastKey])
    }
    where
      lastKey = minBound + len - 1
      len = length as

  toList (LinkedList {..})
    | IM.null container = []
    | otherwise = go firstKey where
      go key = case IM.lookup key container of
        Just (_, value, nextKey) ->
          value : (if nextKey == key then [] else go nextKey)
        _ ->
          []

instance IterLinkedList Integer where

  null (LinkedList {container = cntr}) = M.null cntr

  get iter (LinkedList {..}) =
    fmap (\(prev, value, next) -> value) (M.lookup iter container)

  get' iter (LinkedList {..}) = case M.lookup iter container of
    Just (prev, value, next) -> value
    _ -> undefined

  set iter value list@(LinkedList {..}) = list{container = M.adjust f iter container}
    where
      f (prevKey, _, nextKey) = (prevKey, value, nextKey)

  modify iter f list@(LinkedList {..}) = list{container = M.adjust g iter container}
    where
      g (prevKey, value, nextKey) = (prevKey, f value, nextKey)

  next (LinkedList {..}) iter = case M.lookup iter container of
    Just (prev, value, next) -> next
    _ -> iter

  prev (LinkedList {..}) iter = case M.lookup iter container of
    Just (prev, value, next) -> prev
    _ -> iter

  empty = LinkedList
    { newKey = 0
    , firstKey = 0
    , lastKey = 0
    , container = M.empty
    }

  singleton value = LinkedList
    { newKey = 1
    , firstKey = 0
    , lastKey = 0
    , container = M.singleton 0 (0, value, 0)
    }

  insertBefore iter value (LinkedList {..}) = LinkedList
    { newKey = newKey + 1
    , firstKey = if isPrepend then newKey else firstKey
    , lastKey = lastKey
    , container
      = M.insert newKey (prevKey, value, nextKey)
      $ M.adjust
        (\(prevPrevKey, prevValue, prevNextKey) -> (prevPrevKey, prevValue, newKey))
        prevKey
      $ M.adjust
        (\(nextPrevKey, nextValue, nextNextKey) -> (newKey, nextValue, nextNextKey))
        nextKey
      $ container
    }
    where
      (isPrepend, prevKey, nextKey) = case M.lookup iter container of
        Nothing -> (True, newKey, firstKey)
        Just (iterPrevKey, iterValue, iterNextKey)
          | iterPrevKey == iter -> (True, newKey, iter)
          | otherwise -> (False, iterPrevKey, iter)

  insertAfter iter value (LinkedList {..}) = LinkedList
    { newKey = newKey + 1
    , firstKey = firstKey
    , lastKey = if isAppend then newKey else lastKey
    , container
      = M.insert newKey (prevKey, value, nextKey)
      $ M.adjust
        (\(prevPrevKey, prevValue, prevNextKey) -> (prevPrevKey, prevValue, newKey))
        prevKey
      $ M.adjust
        (\(nextPrevKey, nextValue, nextNextKey) -> (newKey, nextValue, nextNextKey))
        nextKey
      $ container
    }
    where
      (isAppend, prevKey, nextKey) = case M.lookup iter container of
        Nothing -> (True, lastKey, newKey)
        Just (iterPrevKey, iterValue, iterNextKey)
          | iterNextKey == iter -> (True, iter, newKey)
          | otherwise -> (False, iter, iterNextKey)

  delete iter list@(LinkedList {..}) = case M.lookup iter container of
    Nothing -> list
    Just (iterPrevKey, iterValue, iterNextKey) -> LinkedList
      { newKey = newKey
      , firstKey = if firstKey == iter then iterNextKey else firstKey
      , lastKey = if lastKey == iter then iterPrevKey else lastKey
      , container
        = M.adjust (\(prevPrevKey, prevValue, prevNextKey) ->
            (prevPrevKey, prevValue, if iterNextKey == iter then iterPrevKey else iterNextKey)
          ) iterPrevKey
        $ M.adjust (\(nextPrevKey, nextValue, nextNextKey) ->
            (if iterPrevKey == iter then iterNextKey else iterPrevKey, nextValue, nextNextKey)
          ) iterNextKey
        $ M.delete iter container
      }

  fromList [] = empty
  fromList as = LinkedList
    { newKey = len
    , firstKey = 0
    , lastKey = lastKey
    , container = M.fromList $ zip [0..] $ zip3 (0 : [0..]) as ([1 .. lastKey] ++ [lastKey])
    }
    where
      lastKey = len - 1
      len = fromIntegral $ length as

  toList (LinkedList {..})
    | M.null container = []
    | otherwise = go firstKey where
      go key = case M.lookup key container of
        Just (_, value, nextKey) ->
          value : (if nextKey == key then [] else go nextKey)
        _ ->
          []
