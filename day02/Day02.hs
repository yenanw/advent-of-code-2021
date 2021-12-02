module Main where

import Data.List.Split
import Data.Maybe

{-===================PART 1===================-}

data Pos = Pos Int Int
  deriving (Eq, Show)

data Dir = Forward | Down | Up
  deriving (Eq, Show)

data Move = Move Int Dir
  deriving (Eq, Show)

distance :: Move -> Int
distance (Move n _) = n

dir :: Move -> Dir
dir (Move _ d) = d

movePos :: Pos -> Move -> Pos
movePos (Pos x y) (Move n Up) = Pos x (y - n)
movePos (Pos x y) (Move n Down) = Pos x (y + n)
movePos (Pos x y) (Move n Forward) = Pos (x + n) y

day02A :: [Move] -> Int
day02A ms = x * y
  where
    Pos x y = foldl movePos (Pos 0 0) ms

{-===================PART 2===================-}

newtype Aim = Aim Int
  deriving (Eq, Show)

moveAim :: Aim -> Move -> Aim
moveAim (Aim v) (Move n Up) = Aim (v - n)
moveAim (Aim v) (Move n Down) = Aim (v + n)
moveAim aim _ = aim

-- | There are better ways to do it and I chose to do it this way, shame...
moveAimPos :: (Aim, Pos) -> Move -> (Aim, Pos)
moveAimPos (aim, pos) move@(Move n _)
  | dir move == Forward = (Aim v, Pos (x + n) (y + v * n))
  | otherwise = newAimPos
  where
    newAimPos@(Aim v, Pos x y) = (moveAim aim move, pos)

-- | Could've generalised this by creating a higher-order function but
--   that is too much effort.
day02B :: [Move] -> Int
day02B ms = x * y
  where
    (_, Pos x y) = foldl moveAimPos (Aim 0, Pos 0 0) ms

{-===================RESULT===================-}

parse :: String -> [Move]
parse = map readMove . lines

-- | Assuming the string given is always of the correct format,
--   convert thes string to a 'Move'
readMove :: String -> Move
readMove s = readMove' hd tl
  where
    [hd, tl] = take 2 $ splitOn " " s

    readMove' :: String -> String -> Move
    readMove' constr n = Move (read n) (readDir constr)

    readDir :: String -> Dir
    readDir "forward" = Forward
    readDir "down" = Down
    readDir "up" = Up
    readDir _ = error "Wrong format!"

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day02A parsed)
  putStrLn "==========Part 2=========="
  print (day02B parsed)
