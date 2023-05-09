import Data.List (tails)
import Data.Char (ord, chr)
-- This file contains just common algos written in haskell
-- It is just for practise and educational purposes

-- This function finds the maximum in a list
maxlst :: (Ord a) => [a] -> a
maxlst [] = error "Maximum of empty list"
maxlst [x] = x
maxlst (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxlst xs

-- This function will just make a list of replicates of some element
myreplicateHelper :: (Num i, Ord i) => i -> a -> [a] -> [a]
myreplicateHelper n x accum
  | n <= 0 = accum
  | otherwise = myreplicateHelper (n-1) x (x:accum)
myreplicate n x = myreplicateHelper n x []


mytakeHelper :: (Num i, Ord i) => i -> [a] -> [a] -> [a]
mytakeHelper n _ accum
  | n <= 0 = accum
mytakeHelper n [] accum = accum
mytakeHelper n [x] accum = accum ++ [x]
mytakeHelper n (x:xs) accum = mytakeHelper (n-1) xs (accum ++ [x])
mytake n l = mytakeHelper n l []

-- Implementation of quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- Just reverses a list
myreverse :: [a] -> [a]
myreverse [] = []
myreverse [x] = [x]
myreverse (x:xs) = myreverse xs ++ [x]

-- search 
search :: Eq a => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle in
  foldl (\acc x -> (take nlen x == needle) || acc) 
  False (tails haystack)

-- leftpad
leftpadInner :: [Char] -> Char -> Int -> Int -> [Char]
leftpadInner str cchar nchar nlen
  | nchar <= nlen = str
  | otherwise = leftpadInner (cchar:str) cchar (nchar-1) nlen
leftpad str cchar nchar = leftpadInner str cchar nchar (length str)

-- Caesar Cipher
cipherCaesar :: Int -> [Char] -> [Char]
cipherCaesar shift msg =
  let ords = map ord msg
      shifter1 x n = (((n-97)+x) `mod` 26)+97 -- This is how we shift lowercases
      shifter2 x n = (((n-65)+x) `mod` 26)+65 -- This is how we shift uppercases
      maybeShift x n
        | 97 <= n && n <= 122 = shifter1 x n -- This checks if it's a lowercase
        | 65 <= n && n <= 90 = shifter2 x n -- This checks if it's an uppercase
        | otherwise = n
      shifted = map (maybeShift shift) ords
      in map chr shifted -- Transform the Ints back to Chars


-- Find key function using where instead of a lambda
findKey :: (Foldable t, Eq p) => p -> t (p, a) -> Maybe a
findKey key = foldr find Nothing
  where find (k,v) acc = if key == k then Just v else acc

-- Implementing a binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

node :: a -> Tree a
node x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = node x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- Tree generator from a list of nums
treeright :: (Ord a) => [a] -> Tree a
treeright [] = EmptyTree
treeright lst = foldr treeInsert EmptyTree lst

treeleft lst = treeright (reverse lst)
