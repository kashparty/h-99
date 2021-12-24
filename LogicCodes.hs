import Data.List
import Data.Char
import Data.Maybe
and' :: Bool -> Bool -> Bool
and'
  = (&&)

or' :: Bool -> Bool -> Bool
or'
  = (||)

nand' :: Bool -> Bool -> Bool
nand'
  = (not .) . and'

nor' :: Bool -> Bool -> Bool
nor'
  = (not .) . or'

xor' :: Bool -> Bool -> Bool
xor' l r
  = (not l `and'` r) `or'` (l `and'` not r)

impl' :: Bool -> Bool -> Bool
impl' l r
  = not l `or'` r

equ' :: Bool -> Bool -> Bool
equ' l r
  = l `impl'` r && r `impl'` l

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

table :: (Bool -> Bool -> Bool) -> IO ()
table fn
  = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (fn x y)
                    | x <- [True, False], y <- [True, False]]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n fn
  = mapM_ putStrLn [unwords $ map show assignment ++ [show (fn assignment)]
                    | assignment <- assignments n]
  where
    assignments :: Int -> [[Bool]]
    assignments 0
      = [[]]
    assignments n
      = map (True :) (assignments (n - 1)) ++
        map (False :) (assignments (n - 1))

gray :: Int -> [String]
gray 1
  = ["0", "1"]
gray n
  = map ('0' :) prev ++ map ('1' :) (reverse prev)
  where
    prev = gray (n - 1)

huffman :: [(String, Int)] -> [(String, String)]
huffman [(a, _), (b, _)]
  = [(a, "1"), (b, "0")]
huffman freqs
  = [(c, e) | (c, e) <- result , c /= ac ++ bc] ++ [(ac, ace)] ++ [(bc, bce)]
  where
    ((ac, af) : (bc, bf) : rest) = sortOn snd freqs
    result = huffman $ (ac ++ bc, af + bf) : rest
    abe = fromJust $ lookup (ac ++ bc) result
    ace = abe ++ "1"
    bce = abe ++ "0"