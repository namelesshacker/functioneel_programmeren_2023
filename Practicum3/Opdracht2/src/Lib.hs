module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- https://www.educative.io/answers/how-to-remove-duplicates-from-a-list-in-haskell

import Data.Set (fromList, toList)

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

main :: IO ()
main = do
  let inputList = [1, 2, 3, 4, 3, 2, 1]
      outputList = removeDuplicates inputList
  putStrLn "Input List:"
  print inputList
  putStrLn "Output List (after removing duplicates):"
  print outputList

-- https://codereview.stackexchange.com/questions/150533/filter-duplicate-elements-in-haskell


uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)


count e = length . filter (== e)


showList :: Show a => [a] -> String
showList [] = "-1"
showList a = unwords [show i | i <- a]

main :: IO ()
main = do
  a <- readLn
  replicateM_ a $ do
    [_n, k] <- map read . words <$> getLine
    numbers <- map read . words <$> getLine
    putStrLn $ showList $ filt k numbers

-- https://stackoverflow.com/questions/26217871/haskell-function-that-tests-if-a-list-has-repeated-duplicate-elements


import Data.List
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs


-- https://gist.githubusercontent.com/morpheus-14/ac2cd50e385c7720f9166f9f92a6c893/raw/e3a92bf612ff206fc070f50025dd60054395fddd/99problems.hs


module Main where

import Data.List
import Data.Tree
import System.IO


-- Q 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast = last
-- myLast [] = error "Empty List"
-- myLast [x] = x
-- myLast (_:xs) = myLast xs

-----------------------------

-- Q 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = head . tail . reverse

-----------------------------

-- Q 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt lst key = lst !! (key - 1)

----------------------------

-- Q 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength = length

----------------------------

-- Q 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse = reverse

----------------------------

-- Q 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

----------------------------

-- Q 7
-- Flatten a nested list structure.
myFlatten = error "No yet implementation"

----------------------------

-- Q 8
-- Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress = map head . group

----------------------------

-- Q 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack = group

----------------------------

-- Q 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) . group xs

----------------------------

-- Q 11
-- Modified run-length encoding. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

data Encoder a = Single a | Multiple Int a
  deriving (Show)
encodeModified :: (Eq a) => [a] -> [Encoder a]
encodeModified = map encodeHelper . encode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

----------------------------

-- Q 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

decodeModified :: (Eq a) => [Encoder a] -> [a]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

----------------------------

-- Q 13
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

encodeDirect = error "Not yet implemented"

----------------------------

-- Q 14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli = concatMap $ replicate 2

----------------------------

-- Q 15
-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x

----------------------------

-- Q 16
-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\(s, i) -> i `mod` n == 0) $ zip xs [1..]

----------------------------

-- Q 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

----------------------------

-- Q 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice xs i k = map fst $ filter (\(s, j) -> j >= i && j <= k) $ zip xs [1..]

----------------------------

-- Q 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).

rotate :: [a] -> Int -> [a]
rotate xs n = take len . drop (n `mod` len) . cycle $ xs
	where
		len = length xs

----------------------------

-- Q 20
-- Remove the K'th element from a list.

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (key, list)
	where
		key = xs !! n
		list = map fst $ filter (\(x, i) -> i /= n) $ zip xs [0..n]

----------------------------

-- Q 21
-- Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt key list n = (\(x, y) -> x ++ [key] ++ y) $ split list n


----------------------------

-- Q 22
-- Create a list containing all integers within a given range.
myRange :: Int -> Int -> [Int]
myRange = error "No yet implementation"

----------------------------

-- Q 23
-- Extract a given number of randomly selected elements from a list.
rnd_sel = error "No yet implementation"

----------------------------

main = do
  putStrLn (myLast testList)
  putStrLn (myButLast testList)
  putStrLn (elementAt testList 1)
  putStrLn $ show (myLength testList)
  putStrLn $ show (myReverse testList)
  putStrLn $ show (isPalindrome testList)
  putStrLn $ show (isPalindrome palindromeList)
  putStrLn $ show (compress bloatList)
  putStrLn $ show (pack bloatStringList)
  putStrLn $ show (encode newString)
  putStrLn $ show (encodeModified newString)
  putStrLn $ show (decodeModified (encodeModified newString))
  putStrLn $ show (dupli newList)
  putStrLn $ show (repli newList 4)
  putStrLn $ show (dropEvery newList 3)
  putStrLn $ show (split newList 3)
  putStrLn $ show (slice bloatStringList 3 7)
  putStrLn $ show (rotate newList 3)
  putStrLn $ show (removeAt 3 newList)
  putStrLn $ show (insertAt 8 newList 3)
    where
      testList = ["hello", "guys", "its", "mah", "budday"]
      palindromeList = [1, 2, 3, 4, 5, 4, 3, 2, 1]
      bloatList = [1, 1, 2, 4, 1, 5, 5, 2]
      bloatStringList = ['m', 'i', 's', 's', 'i', 's', 's', 'p', 'p', 'e', 'e']
      newString = "aaaabccaadeeee"
      newList = [1, 2, 3, 4, 5, 6]


-- https://www.reddit.com/r/haskell/comments/5qav1r/determining_whether_a_list_contains_duplicates/


import Data.List (sort)

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = hasDuplicates' (sort xs)

hasDuplicates' [] = False
hasDuplicates' (y:[]) = False
hasDuplicates' (y:y':ys)
  | y == y' = True
  | otherwise = hasDuplicates' (y':ys)

