module Day14 where

import           Data.List                      ( group
                                                , isPrefixOf
                                                , nub
                                                , partition
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )

{-===================PART 1===================-}

-- | Naive approach that just runs down the string and inserts
--   a polymer according to the rules. Works for part 1, 
--   good luck using this for part 2 tho.
step :: String -> Map String Char -> String
step (s1 : s2 : r) m = case M.lookup [s1, s2] m of
  Nothing -> s1 : rest
  Just c  -> s1 : c : rest
  where rest = step (s2 : r) m
step s _ = s

stepBy :: (a -> Map String Char -> a) -> Int -> a -> Map String Char -> a
stepBy stepFun n a m = foldl (\k _ -> stepFun k m) a [1 .. n]

day14A :: (String, Map String Char) -> Int
day14A (temp, ins) = maximum s - minimum s
  where s = map length $ group (sort $ stepBy step 10 temp ins)

{-===================PART 2===================-}

-- | Given a polymer sequence, return a map of all pairs of polymer and
--   their occurence count.
pairs :: String -> Map String Int
pairs (c1 : c2 : s) = M.insertWith (+) [c1, c2] 1 (pairs $ c2 : s)
pairs _             = M.empty

insertAllWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a -> Map k a
insertAllWith _ []             m = m
insertAllWith f ((k, v) : kvs) m = M.insertWith f k v res
  where res = insertAllWith f kvs m

-- | Given a pair-occurence map and a pair-insert map and step through.
--   Basically, if "AB" -> "C", then the pair-occurence map will have 
--   one less "AB" pair and one more of "AC" and "CA" each. And it does
--   this for each pair in the pair-occurence map.
step' :: Map String Int -> Map String Char -> Map String Int
step' ps ins = M.foldrWithKey (\k _ ps' -> insert k ps ins ps') ps ps
 where
  insert s ps ins = insertAllWith (+) kvs
   where
    c   = fromJust $ M.lookup s ins
    v   = fromJust $ M.lookup s ps
    kvs = [([head s, c], v), ([c, last s], v), (s, -v)]

-- | Get all possible polymers.
polymers :: Map String b -> [Char]
polymers = nub . concat . M.keys

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

-- | Given a polymer and a map of pattern occurences, count the smallest
--   possible occurence count for the given polymer. Or that is what it
--   is supposed to do, no idea if it actually works...
countOccurence :: Char -> Map String Int -> (Int, Char)
countOccurence c m = (n, c)
 where
  ps        = M.toList $ M.filterWithKey (\k _ -> c `elem` k) m
  (cc, res) = partition ((== [c, c]) . fst) ps
              -- plus 1 here to adjust for remainder
  n         = maybe 0 snd (safeHead cc) + (sum (map snd res) + 1) `div` 2

-- | I have no idea why the smallest possible occurence count is the
--   same as the actually occurence count, but if it works, it works.
occurences :: Map String Int -> [(Int, Char)]
occurences ps = [ countOccurence p ps | p <- polymers ps ]

day14B :: (String, Map String Char) -> Int
day14B (temp, ins) = maximum os - minimum os
 where
  m  = stepBy step' 40 (pairs temp) ins
  os = map fst (occurences m)

{-===================RESULT===================-}

-- | This is so prone to bugs, how did it work...
parse :: String -> (String, Map String Char)
parse s = (head temp, parseToMap ins)
  where [temp, ins] = splitOn [""] (lines s)

parseToMap :: [String] -> Map String Char
parseToMap ss = M.fromList $ map ((\[h, l] -> (h, head l)) . splitOn " -> ") ss

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day14A parsed)
  putStrLn "==========Part 2=========="
  print (day14B parsed)
