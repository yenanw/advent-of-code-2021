module Day07 where

import           Data.List                      ( partition )
import           Data.List.Split                ( splitOn )


{-===================PART 1===================-}

-- | Equation derived from doing some simple math.
--   A bit scuffed tho, since it still is linear in complexity because it
--   takes the absolute value of each crap.
cost :: Int -> [Int] -> Int
cost t ns = sum h - sum l - t * (length h - length l)
  where (l, h) = partition (< t) ns

-- | Given a cost function, a list of positions and return the minimum cost.
minFuel :: (Int -> [Int] -> Int) -> [Int] -> Int
minFuel f ns = minimum $ map (`f` ns) [0 .. maximum ns]

day07A :: [Int] -> Int
day07A = minFuel cost

{-===================PART 2===================-}

-- | The sum of `1+2+3+...+n` is the equal to `(n(n+1))/2`.
stepCost :: Int -> Int
stepCost n = n * (n + 1) `div` 2

cost' :: Int -> [Int] -> Int
cost' t ns = sum ns' where ns' = map (stepCost . (\n -> abs $ n - t)) ns

-- | So even this is kinda brute-forcable? Maybe will optimize later.
day07B :: [Int] -> Int
day07B = minFuel cost'

{-===================RESULT===================-}

parse :: String -> [Int]
parse = map read . splitOn ","

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day07A parsed)
  putStrLn "==========Part 2=========="
  print (day07B parsed)
