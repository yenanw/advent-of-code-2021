module Main where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

{-===================PART 1===================-}

-- | Execute a function N times.
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = f (applyN (n -1) f x)

-- | Given a list of integrals, count how many times each number appears in
--   the list and convert it to a `Map`.
count :: Integral a => [a] -> Map a Int
count ns = count' ns Map.empty
  where
    count' :: Integral a => [a] -> Map a Int -> Map a Int
    count' [] m = m
    count' (n : ns') m = Map.insertWith (+) n 1 (count' ns' m)

-- | Add a key-value pair to a list of key-value pairs, insert the key
--   if it doesn't exist, otherwise add the new value with the old value.
--   This is needed because no built-in function supports the altering of keys.
addValueTo :: Integral a => (a, a) -> [(a, a)] -> [(a, a)]
addValueTo (k, v) [] = [(k, v)]
addValueTo (k, v) ((k', v') : ns)
  | k == k' = (k', v' + v) : ns
  | otherwise = (k', v') : addValueTo (k, v) ns

-- | Simulates what happens to the laternfish population after one day,
--   given that the population is in right format.
step :: Map Int Int -> Map Int Int
step m = Map.fromList ns'
  where
    isNew (k, _) = k == -1
    ns = map (\(k, v) -> (k - 1, v)) (Map.toList m)
    ns' = case find isNew ns of
      Just (k, v) -> (8, v) : addValueTo (6, v) (filter (not . isNew) ns)
      Nothing -> ns

-- | You can brute-force the first one, good luck with that with the
--   the second part tho.
day06A :: [Int] -> Int
day06A = Map.foldr (+) 0 . applyN 80 step . count

{-===================PART 2===================-}

-- | I should've seen this coming...
day06B :: [Int] -> Int
day06B = Map.foldr (+) 0 . applyN 256 step . count

{-===================RESULT===================-}

parse :: String -> [Int]
parse = map read . splitOn ","

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day06A parsed)
  putStrLn "==========Part 2=========="
  print (day06B parsed)
