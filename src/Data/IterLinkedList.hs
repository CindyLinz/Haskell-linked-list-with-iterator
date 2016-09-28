{-|
Module      : Data.IterLinkedList
Description : A pure linked list which is mutable through iterators.
Copyright   : (c) CindyLinz, 2016
License     : MIT
Maintainer  : cindylinz@gmail.com
Portability : portable

A pure linked list with is mutable through iterators.

It's iternally implemented by 'Data.IntMap.Strict.IntMap' or 'Data.Map.Strict.Map' 'Integer',
using 'Int' or 'Integer' as the iterator type respectly.
Most of the operations cost @O(lg N)@.
Each newly inserted element will consume a unique number and never reuse old numbers.
Choose 'Int' one if you're sure that there're no more than 'Int' space times of insertions,
or choose 'Integer' one otherwise.
-}

module Data.IterLinkedList
  ( IterLinkedList (..)
  , LinkedList
  , firstIter
  , lastIter
  ) where

import Data.IterLinkedList.Internal
