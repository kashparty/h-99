import Data.List
isPrime :: Int -> Bool
isPrime n
  | n == 1           = False
  | n == 2 || n == 3 = True
  | even n           = False
  | n `mod` 3 == 0   = False
  | otherwise        = divsA && divsB
  where
    divsA = and [(n `mod` m) /= 0 | m <- [5,11..n `div` 2]]
    divsB = and [(n `mod` m) /= 0 | m <- [7,13..n `div` 2]]

myGCD :: Int -> Int -> Int
myGCD a 0
  = abs a
myGCD a b
  = myGCD b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime
  = ((1 == ) .) . myGCD

totient :: Int -> Int
totient m
  = length $ filter (coprime m) [1..m]

primeFactors :: Int -> [Int]
primeFactors 1
  = []
primeFactors n
  = firstDivisor : primeFactors (n `div` firstDivisor)
  where
    firstDivisor = head [m | m <- [2..n], (n `mod` m) == 0]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n
  = zip uniques counts
  where
    pfs = (group . sort . primeFactors) n
    uniques = map head pfs
    counts = map length pfs

totientImproved :: Int -> Int
totientImproved
  = product . map (\(a, b) -> (a - 1) * a ^ (b - 1)) . primeFactorsMult

primesR :: Int -> Int -> [Int]
primesR
  = (filter isPrime .) . enumFromTo

goldbach :: Int -> (Int, Int)
goldbach m
  = twoSum (primesR 2 m) m
  where
    twoSum :: [Int] -> Int -> (Int, Int)
    twoSum xs
      = twoSum' xs (reverse xs)

    twoSum' :: [Int] -> [Int] -> Int -> (Int, Int)
    twoSum' (x : xs) (y : ys) m
      | x + y < m = twoSum' xs (y : ys) m
      | x + y > m = twoSum' (x : xs) ys m
      | otherwise = (x, y)

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList
  = ((map goldbach . filter even) .) . enumFromTo

goldbachList' :: Int -> Int -> Int -> [(Int, Int)] 
goldbachList' a b c
  = ((filter ((c <) . fst) .) . goldbachList) a b