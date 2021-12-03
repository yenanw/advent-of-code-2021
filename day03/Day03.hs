module Main where

import Data.List

{-===================PART 1===================-}

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x1 (x : xs)
  | x1 == x = 1 + count x1 xs
  | otherwise = count x1 xs

-- | Most common bit. Returns 1 if both bits are equally as common.
mcb :: String -> Bool
mcb str = zeros <= ones
  where
    [zeros, ones] = [count bit str | bit <- ['0', '1']]

-- | Least common bit is just the inverse of the MCB. Returns 0 if both
--   bits are equally as common.
lcb :: String -> Bool
lcb = not . mcb

-- | Convert a list of bits, represented by a list of Bool,
--   to a decimal number.
binToDec :: Integral a => [Bool] -> a
binToDec bs = foldl (\a (b, i) -> boolToIntegral b * (2 ^ i) + a) 0 bs'
  where
    bs' = zip (reverse bs) [0, 1 ..]
    boolToIntegral = fromIntegral . fromEnum

day03A :: [String] -> Int
day03A bs = binToDec gamma * binToDec eps
  where
    bs' = transpose bs
    gamma = map mcb bs'
    eps = map lcb bs'

{-===================PART 2===================-}

toBit :: Char -> Bool
toBit = (== '1')

-- | Return the bit at the given index.
bitAt :: String -> Int -> Bool
bitAt bs i = toBit $ bs !! i

-- | Given a criteria, either 'lcb' or 'mcb', keep filtering the list
--   of bits until there is only one left.
bitCriteria :: (String -> Bool) -> [String] -> String
bitCriteria f bs = criteria bs 0
  where
    criteria :: [String] -> Int -> String
    criteria [s] _ = s
    criteria ss n = criteria filtered (n + 1)
      where
        cols = map f (transpose ss)
        wanted = cols !! n
        compBit = (== wanted) . flip bitAt n
        filtered = filter compBit ss

day03B :: [String] -> Int
day03B ms = binToDec generatorRating * binToDec scrubberRating
  where
    generatorRating = map toBit $ bitCriteria mcb ms
    scrubberRating = map toBit $ bitCriteria lcb ms

{-===================RESULT===================-}

parse :: String -> [String]
parse = lines

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day03A parsed)
  putStrLn "==========Part 2=========="
  print (day03B parsed)
