import Data.Maybe
import Data.List

myLast :: [a] -> a
myLast [x]
  = x
myLast (_ : xs)
  = myLast xs

myButLast :: [a] -> a
myButLast [x, _]
  = x
myButLast (_ : xs)
  = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x : _) 1
  = x
elementAt (_ : xs) k
  = elementAt xs (k - 1)

myLength :: [a] -> Int
myLength
  = foldl ((succ .) . const) 0

myReverse :: [a] -> [a]
myReverse
  = flip myReverse' []
  where
    myReverse' :: [a] -> [a] -> [a]
    myReverse' [] rs
      = rs
    myReverse' (x : xs) rs
      = myReverse' xs (x : rs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs
  = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)
  = [x]
flatten (List xs)
  = concatMap flatten xs

compress :: Eq a => [a] -> [a]
compress [x]
  = [x]
compress (x : xs@(x' : _))
  | x == x'   = compress xs
  | otherwise = x : compress xs

pack :: Eq a => [a] -> [[a]]
pack [x]
  = [[x]]
pack (x : ys@(y : _))
  | x == y    = (x : p) : ps
  | otherwise = [x] : p : ps
  where
    (p : ps) = pack ys

encode :: Eq a => [a] -> [(Int, a)]
encode xs
  = [(length x, head x) | x <- pack xs]

data RLEElement a = Single a | Multiple Int a
                  deriving (Show)

encodeModified :: Eq a => [a] -> [RLEElement a]
encodeModified xs
  = [transform x | x <- pack xs]
  where
    transform [x]
      = Single x
    transform xs@(x : _)
      = Multiple (length xs) x

decodeModified :: [RLEElement a] -> [a]
decodeModified
  = concatMap transform
  where
    transform (Single x)
      = [x]
    transform (Multiple n x)
      = replicate n x

encodeDirect :: Eq a => [a] -> [RLEElement a]
encodeDirect []
  = []
encodeDirect xs@(x : _)
  | repititions == 1 = Single x : encodeDirect rest
  | otherwise        = Multiple repititions x : encodeDirect rest
  where
    (sames, rest) = span (x == ) xs
    repititions = length sames

dupli :: [a] -> [a]
dupli
  = concatMap (replicate 2)

repli :: [a] -> Int -> [a]
repli
  = (. replicate) . (>>=)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n
  = [x | (x, p) <- zip xs pat, p /= n]
  where
        pat = cycle [1..n]

split :: [a] -> Int -> ([a], [a])
split xs 0
  = ([], xs)
split (x : xs) n
  = (x : left, right)
  where
    (left, right) = split xs (n - 1)

slice :: [a] -> Int -> Int -> [a]
slice xs i k
  = map fst $ filter (\(_, x) -> x `elem` [i..k]) $ zip xs [1..]

rotate :: [a] -> Int -> [a]
rotate xs n
  = right ++ left
  where
    (left, right) = splitAt (n `mod` length xs) xs

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
  = (r, left ++ right) 
  where
    (left, r : right) = splitAt (n - 1) xs

insertAt :: a -> [a] -> Int -> [a]
insertAt x' xs 1
  = x' : xs
insertAt x' (x : xs) n
  = x : insertAt x' xs (n - 1)

range :: Int -> Int -> [Int]
range
  = enumFromTo

combinations :: Int -> [a] -> [[a]]
combinations 0 _ 
  = [[]]
combinations _ []
  = []
combinations n (x : xs)
  = map (x :) (combinations (n - 1) xs) ++ combinations n xs

lsort :: Ord a => [[a]] -> [[a]]
lsort xs
  = map snd $ sort $ zip (map length xs) xs

lfsort :: Ord a => [[a]] -> [[a]]
lfsort xs
  = map snd . sort $ zip (map (\x -> lookup (length x) occs) xs) xs
  where
    occs = (map (\ls -> (head ls, length ls)) . group . sort . map length) xs