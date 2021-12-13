module Day13 where
import           Data.List                      ( isPrefixOf
                                                , partition
                                                , stripPrefix
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

{-===================PART 1===================-}

type Pos = (Int, Int)

data FoldInstr = X Int | Y Int
  deriving (Eq, Show)

-- | Fold a single position.
type FoldOne = Int -> Pos -> Pos

-- | Given the folding point and a position, return `True` if
--   the position is before the folding point, else `False`.
type FoldCriteria = Int -> Pos -> Bool

value :: FoldInstr -> Int
value (X n) = n
value (Y n) = n

-- | A very forced higher-order function to generalise folding
--   left or up.
foldBy :: Int -> FoldOne -> FoldCriteria -> Set Pos -> Set Pos
foldBy n fo fc ps = bs `S.union` S.map (fo n) as
  where (bs, as) = S.partition (fc n) ps

foldPosLeft :: FoldOne
foldPosLeft n (x, y) = (2 * n - x, y)

foldPosUp :: FoldOne
foldPosUp n (x, y) = (x, 2 * n - y)

foldOrigami :: FoldInstr -> Set Pos -> Set Pos
foldOrigami (X n) = foldBy n foldPosLeft (\n (x, _) -> x < n)
foldOrigami (Y n) = foldBy n foldPosUp (\n (_, y) -> y < n)

day13A :: (Set Pos, [FoldInstr]) -> Int
day13A (ps, fs) = length $ foldOrigami (head fs) ps

{-===================PART 2===================-}

border :: [Pos] -> Pos
border ps = (maximum xs, maximum ys) where (xs, ys) = unzip ps

-- | Print all positions out. To be honst I'm confused how this works.
showAllPos :: Set Pos -> String
showAllPos ps = unlines
  [ [ if (x, y) `S.member` ps then '#' else '.' | x <- [0 .. c] ]
  | y <- [0 .. r]
  ]
  where (c, r) = border (S.toList ps)

-- | Must `foldl` because of the ordering.
day13B :: (Set Pos, [FoldInstr]) -> String
day13B (ps, fs) = showAllPos ps' where ps' = foldl (flip foldOrigami) ps fs

{-===================RESULT===================-}

-- | Parsing sure is a lot of fun...
parse :: String -> (Set Pos, [FoldInstr])
parse ss =
  (S.fromList $ map parsePos (filter (not . null) ps), map parseFoldInstr fs)
  where (fs, ps) = partition (isPrefixOf "fold along") (lines ss)

parsePos :: String -> Pos
parsePos s = (read x, read y) where [x, y] = splitOn "," s

parseFoldInstr :: String -> FoldInstr
parseFoldInstr s | i == 'x'  = X n
                 | i == 'y'  = Y n
                 | otherwise = error "Ey, come on my man!"
 where
  (i : '=' : ss) = fromJust $ stripPrefix "fold along " s
  n              = read ss

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day13A parsed)
  putStrLn "==========Part 2=========="
  putStrLn (day13B parsed)
