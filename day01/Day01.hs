module Main where

import Data.Maybe

{-===================PART 1===================-}

-- | If all numbers are increasing the solution to this answer is simply
--   the length of the list of numbers.
day01A :: [Int] -> Int
day01A = length . catMaybes . filterDecreasing

filterDecreasing :: [Int] -> [Maybe Int]
filterDecreasing = filterDecreasing' []
  where
    filterDecreasing' acc [] = acc
    filterDecreasing' acc [x] = acc ++ [isIncreasing (fromMaybe 0 $ last acc) x]
    filterDecreasing' acc (x1 : x2 : xs) = filterDecreasing' acc' (x2 : xs)
      where
        acc' = acc ++ [isIncreasing x1 x2]

isIncreasing :: Ord a => a -> a -> Maybe a
isIncreasing a b
  | b > a = Just b
  | otherwise = Nothing

{-===================PART 2===================-}

day01B :: [Int] -> Int
day01B = day01A . map sumThree . threeMeasurements

threeMeasurements :: [a] -> [(a, a, a)]
threeMeasurements xs = zip3 xs (tail xs) (drop 2 xs)

sumThree :: Num a => (a, a, a) -> a
sumThree (x1, x2, x3) = x1 + x2 + x3

{-===================RESULT===================-}

parse :: String -> [Int]
parse = map read . lines

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day01A parsed)
  putStrLn "==========Part 2=========="
  print (day01B parsed)
