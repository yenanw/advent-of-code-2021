module Day11 where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.List                      ( (\\) )
import           Data.Vector                    ( (!)
                                                , (//)
                                                , Vector
                                                )
import qualified Data.Vector                   as V

{-===================PART 1===================-}

type Dumbo = Vector (Vector Int)

type Pos = (Int, Int)

-- | For manual debugging...
showDumbo :: Dumbo -> String
showDumbo = unlines . V.toList . V.map (V.toList . V.map intToDigit)

at :: Dumbo -> Pos -> Int
at d (y, x) = d ! y ! x

update :: Dumbo -> [(Pos, Int)] -> Dumbo
update d []             = d
update d ((p, v) : pvs) = update (update' d p v) pvs
 where
  update' :: Dumbo -> Pos -> Int -> Dumbo
  update' d (y, x) v = d // [(y, d ! y // [(x, v)])]

updateWith :: (Int -> Int) -> Dumbo -> [Pos] -> Dumbo
updateWith f d ps = update d (ps `zip` vs) where vs = map (f . at d) ps

allPos :: [Pos]
allPos = [ (y, x) | y <- [0 .. 9], x <- [0 .. 9] ]

neighbors :: Pos -> [Pos]
neighbors (y, x) =
  [ (y', x')
  | y' <- [y - 1, y, y + 1]
  , x' <- [x - 1, x, x + 1]
  , inRange y' && inRange x' && (y, x) /= (y', x')
  ]
  where inRange n = n >= 0 && n < 10

flash :: Dumbo -> Pos -> Dumbo
flash d p = updateWith (+ 1) d ps where ps = neighbors p

flashAll :: Dumbo -> [Pos] -> Dumbo
flashAll = foldl flash

resetFlash :: Dumbo -> Dumbo
resetFlash = mapAll (\v -> if v > 9 then 0 else v)

mapAll :: (Int -> Int) -> Dumbo -> Dumbo
mapAll f = V.map (V.map f)

findAll :: (Int -> Bool) -> Dumbo -> [Pos]
findAll pred d = [ p | p <- allPos, pred (d `at` p) ]

step :: Dumbo -> (Int, Dumbo)
step db = step' (mapAll (+ 1) db) []
 where
  step' :: Dumbo -> [Pos] -> (Int, Dumbo)
  step' d ps | null fs   = (length ps, resetFlash d)
             | otherwise = step' d' (fs ++ ps)
   where
    fs = findAll (> 9) d \\ ps
    d' = flashAll d fs

stepBy :: Int -> Dumbo -> (Int, Dumbo)
stepBy 0 d = (0, d)
stepBy n d = (c + c', r)
 where
  (c , d') = step d
  (c', r ) = stepBy (n - 1) d'

day11A :: Dumbo -> Int
day11A = fst . stepBy 100

{-===================PART 2===================-}

isSynced :: Dumbo -> Bool
isSynced = null . findAll (/= 0)

-- | Just run it until it's synced I guess...
syncPoint :: Dumbo -> Int
syncPoint db = syncPoint' db 1
 where
  syncPoint' :: Dumbo -> Int -> Int
  syncPoint' d c | isSynced d' = c
                 | otherwise   = syncPoint' d' (c + 1)
    where d' = snd (step d)

day11B :: Dumbo -> Int
day11B = syncPoint

{-===================RESULT===================-}

parse :: String -> Dumbo
parse = V.fromList . map (V.fromList . map digitToInt) . lines

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day11A parsed)
  putStrLn "==========Part 2=========="
  print (day11B parsed)
