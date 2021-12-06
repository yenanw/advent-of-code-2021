module Main where

import Data.List
import Data.List.Split
import Data.Maybe

{-===================PART 1===================-}

-- | Simple data type to reprent a number on a Bingo card,
--   it is either chosen or not chosen. (Marked is probably a better name...)
data Choice a = Choice a Bool
  deriving (Show, Eq)

choice :: a -> Choice a
choice v = Choice v False

chosen :: Choice a -> Bool
chosen (Choice _ c) = c

value :: Choice a -> a
value (Choice v _) = v

choose :: Choice a -> Choice a
choose (Choice v _) = Choice v True

allChosen :: [Choice a] -> Bool
allChosen = all chosen

match :: Eq a => a -> Choice a -> Choice a
match v ch
  | value ch == v = choose ch
  | otherwise = ch

newtype Bingo = Bingo [[Choice Int]]
  deriving (Show, Eq)

isBingo :: Bingo -> Bool
isBingo (Bingo css) = length css == 5 && all ((== 5) . length) css

choices :: Bingo -> [[Choice Int]]
choices (Bingo css) = css

numbers :: Bingo -> [Int]
numbers (Bingo css) = concatMap (map value) css

getUnmarked :: Bingo -> [Choice Int]
getUnmarked bg = filter (not . chosen) (concat $ choices bg)

type Block = [Choice Int]

blocks :: Bingo -> [Block]
blocks (Bingo css) = css ++ transpose css

-- | Draw a number, a.k.a. play a turn in this Bingo game.
draw :: Int -> Bingo -> Bingo
draw n (Bingo css) = Bingo $ map (map $ match n) css

-- | Given a Bingo, return the row or column that's all marked
winningRow :: Bingo -> Maybe [Int]
winningRow bg = map value <$> find allChosen (blocks bg)

-- | Given a list of Bingo, return a tuple of the first winner and the
--   rest of Bingos.
winner :: [Bingo] -> (Maybe Bingo, [Bingo])
winner [] = (Nothing, [])
winner (b : bg)
  | isJust (winningRow b) = (Just b, bg)
  | otherwise = (mbg, b : rest)
  where
    (mbg, rest) = winner bg

-- | Given a list of numbers to draw, and a list of Bingo cards, play
--   until either draw list is exhausted or the all Bingos won.
playBingo :: [Int] -> [Bingo] -> [(Bingo, Int)]
playBingo _ [] = []
playBingo [] _ = []
playBingo (n : ns) bgs = case winner bgs' of
  (Nothing, _) -> playBingo ns bgs'
  -- Found a winner, keep finding next winner on the same number util 
  -- there are no more winners from this number
  (Just bingo, rest) -> (bingo, n) : playBingo (n:ns) rest
  where
    bgs' = map (draw n) bgs

day04A :: [Int] -> [Bingo] -> Int
day04A drawList bgs = sum ns * n
  where
    (bg, n) = head $ playBingo drawList bgs
    ns = map value (getUnmarked bg)

{-===================PART 2===================-}

-- | Just take the last to win here, I guess...
day04B :: [Int] -> [Bingo] -> Int
day04B drawList bgs = sum ns * n
  where
    (bg, n) = last $ playBingo drawList bgs
    ns = map value (getUnmarked bg)

{-===================RESULT===================-}

-- | I should probably use a proper parser for this...
--   This is not gonna work if the input is just slightly wrong...
parse :: String -> ([Int], [Bingo])
parse str = (parseDrawList dl, parseBingos ss)
  where
    (dl : ss) = lines str

parseDrawList :: String -> [Int]
parseDrawList = map read . splitOn ","

parseBingos :: [String] -> [Bingo]
parseBingos ss = filter isBingo $ map parseBingo bgss
  where
    bgss = splitOn [""] ss

parseBingo :: [String] -> Bingo
parseBingo ss = Bingo $ map (map (choice . read)) nss
  where
    nss = map words ss

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (drawList, bingos) = parse input
  putStrLn "==========Part 1=========="
  print (day04A drawList bingos)
  putStrLn "==========Part 2=========="
  print (day04B drawList bingos)
