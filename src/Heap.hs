module Heap(
  LeftistHeap,
  empty,
  insert,
  findMin,
  mergeHeaps,
  deleteMin,
  toList
) where

data LeftistHeap a = Empty | NonEmpty { 
  rankNum :: Int,
  x :: a,
  left :: LeftistHeap a,
  right ::  LeftistHeap a
} deriving Show

empty :: Ord a => LeftistHeap a
empty = Empty

insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insert el heap = mergeHeaps (NonEmpty 1 el Empty Empty) heap

rank :: LeftistHeap a -> Int
rank (NonEmpty r _ _ _) = r
rank Empty = 0

makeTree :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeTree el one other = if rank one >= rank other
  then NonEmpty (rank other + 1) el one other
  else NonEmpty (rank one + 1) el other one

mergeHeaps :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
mergeHeaps one Empty = one
mergeHeaps Empty other = other
mergeHeaps one@(NonEmpty _ el1 l1 r1) other@(NonEmpty _ el2 l2 r2) =
  if el1 < el2
  then makeTree el1 l1 (mergeHeaps r1 other)
  else makeTree el2 l2 (mergeHeaps r2 one)

findMin :: Ord a => LeftistHeap a -> Maybe a
findMin Empty = Nothing
findMin (NonEmpty _ e _ _) = Just e

deleteMin :: Ord a => LeftistHeap a -> Maybe (a, LeftistHeap a)
deleteMin Empty = Nothing
deleteMin (NonEmpty _ e l r) = Just (e, mergeHeaps l r)

toList :: LeftistHeap a -> [a]
toList Empty = []
toList (NonEmpty _ a l r) = a : toList r ++ toList l

