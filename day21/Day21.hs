module Day21 where

import           Data.List                      ( group
                                                , nub
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M

{-===================PART 1===================-}

-- | A player pawn, first parameter being the current position,
--   and second being the total score
data Pawn = Pawn Int Int
  deriving (Eq, Ord, Show)

-- | Creates a new pawn starting at the given position.
pawn :: Int -> Pawn
pawn p = Pawn p 0

pos :: Pawn -> Int
pos (Pawn p _) = p

score :: Pawn -> Int
score (Pawn _ s) = s

move :: Int -> Pawn -> Pawn
move n (Pawn p s) = Pawn p' (s + p')
 where
  p' = case (p + n) `mod` 10 of
    0     -> 10
    other -> other

rollDeterministic :: Int -> (Int, Int)
rollDeterministic n = (n + n2 + n3, n3 + 1)
 where
  next d = d + 1 `mod` 100
  n2 = next n
  n3 = next n2

-- | I'm just gonna do something real scuffed here.
deterministicGame :: Pawn -> Pawn -> Int
deterministicGame = game 1 0
 where
  game :: Int -> Int -> Pawn -> Pawn -> Int
  game n c p1@(Pawn _ s1) p2@(Pawn _ s2)
    | s1 >= 1000 || s2 >= 1000 = min s1 s2 * c
    | even c                   = game n' c' (move s p1) p2
    | otherwise                = game n' c' p1 (move s p2)
   where
    c'      = c + 3
    (s, n') = rollDeterministic n

day21A :: (Pawn, Pawn) -> Int
day21A (p1, p2) = deterministicGame p1 p2

{-===================PART 2===================-}

-- | Since there are no references so can't really use two separate maps...
type Pair = (Pawn, Pawn)

-- | All possible outcomes in a single turn. 
rollQuantum :: [(Int, Int)]
rollQuantum = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
-- rollQuantum = map (\xs -> (head xs, length xs)) $ (group . sort)
--   [ a + b + c | a <- [1, 2, 3], b <- [1, 2, 3], c <- [1, 2, 3] ]

-- | Convenience function for adding two 2-Int tuples together.
sumTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuple (a, b) (a', b') = (a + a', b + b')

-- | Given a map of a pair and it's occurence, a boolean stating whose turn
--   it is (True being the first player and vice versa), return the updated
--   map after a single dice roll.
moveQuantum :: Map Pair Int -> Bool -> Map Pair Int
moveQuantum m first = M.fromListWith
  (+)
  [ if first then ((move s p1, p2), f * occ) else ((p1, move s p2), f * occ)
  | ((p1, p2), occ) <- M.toList m
  , (s       , f  ) <- rollQuantum
  ]

hasWonQuantum :: Pair -> Bool
hasWonQuantum (Pawn _ s1, Pawn _ s2) = s1 >= 21 || s2 >= 21

scores :: Pair -> Int -> (Int, Int)
scores (p1@(Pawn _ s1), p2) occ | s1 >= 21  = (occ, 0)
                                | otherwise = (0, occ)

winnerScores :: Map Pair Int -> (Int, Int)
winnerScores = M.foldrWithKey (\k v a -> sumTuple (scores k v) a) (0, 0)

filterByWinner :: Map Pair Int -> ((Int, Int), Map Pair Int)
filterByWinner m = (winnerScores winners, noWinners)
  where (winners, noWinners) = M.partitionWithKey (\p _ -> hasWonQuantum p) m

turn :: Map Pair Int -> ((Int, Int), Map Pair Int)
turn m = (sumTuple ss ss', m'')
 where
  (ss , m' ) = filterByWinner (moveQuantum m True)
  (ss', m'') = filterByWinner (moveQuantum m' False)

-- | Play a turn, count all occurences of games that is over and return their
--   respective scores and do it over and over again until no games are left.
quantumGame :: Map Pair Int -> (Int, Int)
quantumGame = qGame (0, 0)
 where
  qGame :: (Int, Int) -> Map Pair Int -> (Int, Int)
  qGame ss m | M.null m  = ss
             | otherwise = let (ss', m') = turn m in qGame (sumTuple ss ss') m'

day21B :: (Pawn, Pawn) -> Int
day21B pp = max s1 s2 where (s1, s2) = quantumGame (M.singleton pp 1)

{-===================RESULT===================-}

-- | Yes.
parse :: String -> (Pawn, Pawn)
parse s = (pawn p1, pawn p2)
  where [p1, p2] = map (read . (!! 1) . splitOn ": ") $ lines s

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day21A parsed)
  putStrLn "==========Part 2=========="
  print (day21B parsed)
