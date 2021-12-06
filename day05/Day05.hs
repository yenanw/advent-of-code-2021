module Main where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map.Strict as Map

{-===================PART 1===================-}

type Pos = (Int, Int)

data Line = Line Pos Pos
  deriving (Eq, Ord, Show)

isVertical :: Line -> Bool
isVertical (Line (x, _) (x', _)) = x == x'

isHorizontal :: Line -> Bool
isHorizontal (Line (_, y) (_, y')) = y == y'

isDiagonal :: Line -> Bool
isDiagonal (Line (x, y) (x', y')) = abs (x' - x) == abs (y' - y)

-- | Scuffed band-aid solution used to generate the range [from..to],
--   because it's dumb and doesn't recognize negative step size by default.
range :: Integral a => a -> a -> [a]
range from to
  | to - from < 0 = [from, from - 1 .. to]
  | otherwise = [from .. to]

-- | Given a line and return all coordinates on the line.
points :: Line -> [Pos]
points l@(Line (x, y) (x', y'))
  | isHorizontal l = zip xs (repeat y)
  | isVertical l = zip (repeat x) ys
  | otherwise = zip xs ys
  where
    xs = range x x'
    ys = range y y'

-- | This is a typical problem that can be solved trivially using map.
addPos :: Pos -> Map Pos Int -> Map Pos Int
addPos p = Map.insertWith (+) p 1

addLine :: Line -> Map Pos Int -> Map Pos Int
addLine l m = foldr addPos m (points l)

day05A :: [Line] -> Int
day05A ls = Map.size . Map.filter (>= 2) $ foldr addLine Map.empty noDiagonals
  where
    noDiagonals = filter (not . isDiagonal) ls

{-===================PART 2===================-}

-- | This is pretty much the same as part 1, but easier...
day05B :: [Line] -> Int
day05B = Map.size . Map.filter (>= 2) . foldr addLine Map.empty

{-===================RESULT===================-}

-- | I gave up trying to properly parse this.
parse :: String -> [Line]
parse str = map (parseLine . splitOn " -> ") ss
  where
    ss = lines str

parseLine :: [String] -> Line
parseLine [from, to] = Line (parsePos from) (parsePos to)
parseLine _ = error "Wrong format blyaaaat"

parsePos :: String -> Pos
parsePos str = (x, y)
  where
    [x, y] = map read $ splitOn "," str

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print $ day05A parsed
  putStrLn "==========Part 2=========="
  print $ day05B parsed
