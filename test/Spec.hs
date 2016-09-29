{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import qualified Data.IterLinkedList as L

test :: forall a. L.IterLinkedList a => [L.LinkedList a Int]
test =
  let
    list1 :: L.LinkedList a Int
    list1 = go 1 (L.firstIter L.empty) L.empty where
      go 21 _ list = list
      go value iter list = go (value + 1) iter (L.insertBefore iter value list)

    list2 :: L.LinkedList a Int
    list2 = go 1 (L.firstIter L.empty) L.empty where
      go 21 _ list = list
      go value iter list = go (value + 1) iter (L.insertAfter iter value list)

    list3 :: L.LinkedList a Int
    list3 = go 1 (L.firstIter L.empty) L.empty where
      go 21 _ list = list
      go value iter list =
        let
          list' = L.insertBefore iter value list
          iter' = L.prev list' iter
        in
          go (value + 1) iter' list'

    list4 :: L.LinkedList a Int
    list4 = go 1 (L.firstIter L.empty) L.empty where
      go 21 _ list = list
      go value iter list =
        let
          list' = L.insertAfter iter value list
          iter' = L.next list' iter
        in
          go (value + 1) iter' list'

    list5 :: L.LinkedList a Int
    list5 = go 1 L.empty where
      go 21 list = list
      go value list = go (value + 1) (L.insertBefore (L.firstIter list) value list)

    list6 :: L.LinkedList a Int
    list6 = go 1 L.empty where
      go 21 list = list
      go value list = go (value + 1) (L.insertAfter (L.lastIter list) value list)

    list7 :: L.LinkedList a Int
    list7 = go 1 (L.firstIter list5) list5 where
      go 21 _ list = list
      go step iter list = go (step + 2) (L.next list' iter) list'
        where
          list' = L.delete (L.next list iter) list

    list8 :: L.LinkedList a Int
    list8 = go 1 (L.lastIter list6) list6 where
      go 21 _ list = list
      go step iter list = go (step + 2) (L.prev list' iter) list'
        where
          list' = L.delete (L.prev list iter) list

    list9 = go 1 (L.firstIter list6) list6 where
      go 21 _ list = list
      go step iter list = go (step + 1) (L.next list' iter) list'
        where
          list' = L.set iter 7 list

    list10 = go 1 (L.firstIter list6) list6 where
      go 21 _ list = list
      go step iter list = go (step + 1) (L.next list' iter) list'
        where
          list' = L.modify iter (+1) list
  in
    [list1, list2, list3, list4, list5, list6, list7, list8, list9, list10]

answers :: Num a => [[a]]
answers =
  [ [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,1]
  , [1,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2]
  , [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
  , [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
  , [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
  , [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
  , [20,18,16,14,12,10,8,6,4,2]
  , [2,4,6,8,10,12,14,16,18,20]
  , [7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
  , [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]
  ]

getForward :: forall a. (Eq a, L.IterLinkedList a) => L.LinkedList a Int -> [Int]
getForward list = go (L.firstIter list) where
  go iter = L.get' iter list : next where
    iter' = L.next list iter
    next
      | iter' == iter = []
      | otherwise = go iter'

getBackward :: forall a. (Eq a, L.IterLinkedList a) => L.LinkedList a Int -> [Int]
getBackward list = go (L.lastIter list) where
  go iter = L.get' iter list : prev where
    iter' = L.prev list iter
    prev
      | iter' == iter = []
      | otherwise = go iter'

testSet :: forall a. (Show (L.LinkedList a Int), Eq a, L.IterLinkedList a) => [L.LinkedList a Int] -> IO ()
testSet ls = do
  when (map L.toList ls /= answers) $ do
    putStrLn $ show ls ++ " (Int) against " ++ show answers
    exitFailure

  when (map getForward ls /= answers) $ do
    putStrLn $ show ls ++ " (Int forward) against " ++ show answers
    exitFailure

  when (map getBackward ls /= map reverse answers) $ do
    putStrLn $ show ls ++ " (Int backward) against " ++ show (map reverse answers)
    exitFailure

main :: IO ()
main = do
  testSet (test :: [L.LinkedList Int Int])
  testSet (test :: [L.LinkedList Integer Int])
  exitSuccess
