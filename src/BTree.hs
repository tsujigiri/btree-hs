module BTree (
    BTree,
    empty,
    insert,
    member,
    balance,
    fromList,
    values,
) where

import Data.Maybe
import Data.List (sort)

data BTree a = BTree {
    value :: Maybe a,
    left :: BTree a,
    right :: BTree a
}

instance (Eq a, Show a) => Show (BTree a) where
    show tree
        | value == Nothing = "*"
        | otherwise = show (fromJust value) ++ ">" ++ show right ++ "<" ++ show left
        where BTree { value = value, left = left, right = right } = tree

instance (Eq a) => Eq (BTree a) where
    treeA == treeB = valueA == valueB
                     && (value leftA == Nothing && value leftB == Nothing || leftA == leftB)
                     && (value rightA == Nothing && value rightB == Nothing || rightA == rightB)
        where BTree { value = valueA, left = leftA, right = rightA } = treeA
              BTree { value = valueB, left = leftB, right = rightB } = treeB

empty :: BTree a
empty = BTree {
    value = Nothing,
    left = BTree { value = Nothing, left = empty, right = empty },
    right = BTree { value = Nothing, left = empty, right = empty }
}

insert :: (Eq a, Ord a) => a -> BTree a -> BTree a
insert newValue tree
    | value == Nothing = empty { value = Just newValue }
    | newValue == fromJust value = tree
    | newValue < fromJust value = tree { left = insert newValue left }
    | newValue > fromJust value = tree { right = insert newValue right }
    where BTree { value = value, left = left, right = right } = tree

member :: (Eq a, Ord a) => a -> BTree a -> Bool
member lookupValue tree
    | value == Nothing = False
    | lookupValue == fromJust value = True
    | lookupValue < fromJust value = member lookupValue left
    | lookupValue > fromJust value = member lookupValue right
    where BTree { value = value, left = left, right = right } = tree

fromList :: (Ord a) => [a] -> BTree a
fromList = foldl (flip insert) empty

balance :: Ord a => BTree a -> BTree a
balance tree = foldl (flip insert) empty $ balanceValues . sort . values $ tree

balanceValues :: [a] -> [a]
balanceValues [] = []
balanceValues values = pivot:((balanceValues firstHalf) ++ (balanceValues secondHalf))
    where (firstHalf, pivot:secondHalf) = splitAt (length values `div` 2) values

values :: (Eq a) => BTree a -> [a]
values tree
    | value == Nothing = []
    | otherwise = values left ++ [fromJust value] ++ values right
    where BTree { value = value, left = left, right = right } = tree
