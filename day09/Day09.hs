module Day09 where

import           Data.List                      ( sortBy )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                )
import           Data.Set                       ( Set
                                                , notMember
                                                )
import qualified Data.Set                      as S
import           Data.Vector                    ( (!)
                                                , (!?)
                                                , Vector
                                                )
import qualified Data.Vector                   as V

{-===================PART 1===================-}

-- | Models the 2D array of the heightmap. Uses `Vector` because eFfiCIEncY.
type HeightMap = Vector (Vector Int)

type Pos = (Int, Int)

-- | Modified `(!?)` so that it can be chained together.
(?!?) :: Maybe (Vector a) -> Int -> Maybe a
(?!?) Nothing  _ = Nothing
(?!?) (Just v) n = v !? n

-- | 2D version of `(!?)`.
(!!?) :: Vector (Vector a) -> Pos -> Maybe a
(!!?) hm (y, x) = hm !? y ?!? x

-- | 2D version of `(!)`, unsafe.
(!+) :: Vector (Vector a) -> Pos -> a
(!+) hm (y, x) = hm ! y ! x

-- | Return all nearby positions, namely up, down, left and right.
nearbyPos :: Pos -> [Pos]
nearbyPos (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

-- | Return all possible adjacent positions.
adjacent :: Pos -> HeightMap -> [Pos]
adjacent p hm = [ p' | p' <- nearbyPos p, isJust $ hm !!? p' ]

-- | Return all postitions on the height map.
allPos :: HeightMap -> [Pos]
allPos hm = [ (y, x) | y <- [0 .. r], x <- [0 .. c] ]
 where
  r = V.length hm - 1
  c = V.length (hm ! 0) - 1

isLowPoint :: Pos -> HeightMap -> Bool
isLowPoint p hm = all ((v <) . (hm !+)) $ adjacent p hm where v = hm !+ p

lowPoints :: HeightMap -> [Pos]
lowPoints hm = filter (`isLowPoint` hm) (allPos hm)

day09A :: HeightMap -> Int
day09A hm = sum $ map ((+ 1) . fromJust . (hm !!?)) lps
  where lps = lowPoints hm

{-===================PART 2===================-}

-- | Given a position and a set of positions, return all adjacent
--   positions that are not in the set and is valid.
validAdjacent :: Pos -> Set Pos -> HeightMap -> Set Pos
validAdjacent p s hm = S.filter (`isValid` s) ps'
 where
  isValid p' s' = p' `S.notMember` s' && isJust (hm !!? p') && hm !+ p' /= 9
  ps' = S.fromList $ adjacent p hm

-- | Given a position and a height map, generate a basin starting from
--   the given position.
basin :: Pos -> HeightMap -> Set Pos
basin p = basin' (S.singleton p) S.empty
 where
  basin' :: Set Pos -> Set Pos -> HeightMap -> Set Pos
  basin' ps s hm | S.null ps = s
                 | otherwise = basin' pss (s `S.union` pss) hm
    where pss = S.unions $ S.map (\p' -> validAdjacent p' s hm) ps

-- | It should find all basins on a height map. Maybe...
basins :: HeightMap -> Set (Set Pos)
basins hm = basins' (S.fromList $ lowPoints hm) S.empty hm
 where
  basins' :: Set Pos -> Set (Set Pos) -> HeightMap -> Set (Set Pos)
  basins' s ss _ | S.null s = ss
  basins' s ss hm           = basins' (s S.\\ S.insert p bs) ss' hm
   where
    p   = S.findMin s
    bs  = basin p hm
    ss' = S.insert bs ss

-- | Compare by length.
cmpBySize :: Ord a => [a] -> [a] -> Ordering
cmpBySize a b | length a < length b = GT
              | length a > length b = LT
              | otherwise           = EQ

day09B :: HeightMap -> Int
day09B hm = product $ map length bs
  where bs = take 3 $ sortBy cmpBySize (map S.toList $ S.toList (basins hm))

{-===================RESULT===================-}

-- | I love me some unreadable one liners.
parse :: String -> HeightMap
parse = V.fromList . map (V.fromList . map (read . (: []))) . lines

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day09A parsed)
  putStrLn "==========Part 2=========="
  print (day09B parsed)
