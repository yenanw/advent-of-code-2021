module Day10 where

import           Data.List                      ( (\\)
                                                , elemIndex
                                                , partition
                                                , sort
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                , mapMaybe
                                                )

{-===================PART 1===================-}

opening :: [Char]
opening = ['(', '[', '{', '<']

closing :: [Char]
closing = [')', ']', '}', '>']

-- | Given a opening/closing bracket, return the corresponding 
--   closing/opening bracket.
expect :: Char -> Char
expect c | c `elem` opening = expect' opening closing c
         | otherwise        = expect' closing opening c
 where
  expect' :: String -> String -> Char -> Char
  expect' ss1 ss2 c = ss2 !! fromJust (elemIndex c ss1)

-- | Given a chunk, return a tuple of the character that corrupted the
--   chunk (if it is corrupted) and the remainder.
validate :: String -> (Maybe Char, String)
validate s = validate' s []
 where
  validate' :: String -> String -> (Maybe Char, String)
  validate' [] os = (Nothing, os)
  validate' (c : cs) os | c `elem` opening      = validate' cs (c : os)
                        | c == expect (head os) = validate' cs (tail os)
                        | otherwise             = (Just c, cs)

illegalPoints :: Map Char Int
illegalPoints = M.fromList (closing `zip` [3, 57, 1197, 25137])

day10A :: [String] -> Int
day10A = sum . map (\c -> fromJust $ M.lookup c illegalPoints) . mapMaybe
  (fst . validate)

{-===================PART 2===================-}

autoCompletePoints :: Map Char Int
autoCompletePoints = M.fromList (closing `zip` [1, 2, 3, 4])

calcScore :: String -> Int
calcScore ss = foldl ((+) . (* 5)) 0 ns
  where ns = map (fromJust . (`M.lookup` autoCompletePoints)) ss

-- | Scuffed median function that only works for odd lenghted lists.
median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

-- | This makes perfect sense, yes...
day10B :: [String] -> Int
day10B =
  median
    . map (calcScore . (map expect . snd))
    . filter (isNothing . fst)
    . map validate

{-===================RESULT===================-}

-- | Thank you.
parse :: String -> [String]
parse = lines

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day10A parsed)
  putStrLn "==========Part 2=========="
  print (day10B parsed)
