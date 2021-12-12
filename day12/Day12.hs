module Day12 where

import           Data.Graph                     ( Graph
                                                , Vertex
                                                )
import qualified Data.Graph                    as G
import           Data.List.Split                ( splitOn )

import           Data.Char                      ( isLower )
import           Data.List                      ( (\\)
                                                , nub
                                                )
import           Data.Maybe                     ( fromJust )

{-===================PART 1===================-}

-- | Synonym for the hecking graph representation in Haskell.
--   WHY IS GRAPH LIKE THIS IN HASKELL!
type GraphRepr
  = (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)

-- | Given all reachable nodes and visited nodes, return the nodes
-- | that are possible to reach, based on whatever policy.
type GetPossibleNodes = [String] -> [String] -> [String]

-- | Simply check if a given node is a small cave or not.
isSmallCave :: String -> Bool
isSmallCave = all isLower

reachable
  :: (Vertex -> (String, String, [String]))
  -> (String -> Maybe Vertex)
  -> String
  -> [String]
reachable nfv vfk k = ps where (_, _, ps) = nfv $ fromJust (vfk k)

-- | Policy: a small cave can only be visited once.
visitSmallOnce :: GetPossibleNodes
visitSmallOnce ps vs = ps \\ vs

-- | Just walk down every single possible path from start to end.
countAllPaths :: GraphRepr -> GetPossibleNodes -> Int
countAllPaths (g, nfv, vfk) possibleNodes = walkDown "start" ["start"]
 where
  walkDown :: String -> [String] -> Int
  walkDown p vs | p == "end" = 1
                | null ps    = 0
                | otherwise  = sum [ walkDown p' (visit p') | p' <- ps ]
   where
    ps = possibleNodes (reachable nfv vfk p) vs
    -- This shouldn't be needed but IDC.
    visit n = if isSmallCave n then n : vs else vs

day12A :: GraphRepr -> Int
day12A g = countAllPaths g visitSmallOnce

{-===================PART 2===================-}

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- | Policy: a single small cave can be visited twice, every other caves
--           can be visited only once.
visitSmallTwice :: GetPossibleNodes
visitSmallTwice ps vs | visitedTwice vs = visitSmallOnce ps vs
                      | otherwise       = ps \\ ["start"] -- Not allowed to go back to start.
  where visitedTwice xs = any ((>= 2) . (`count` xs)) xs

day12B :: GraphRepr -> Int
day12B g = countAllPaths g visitSmallTwice

{-===================RESULT===================-}

parse :: String -> GraphRepr
parse = mkGraph . map edge . lines
  where edge s = let [v1, v2] = splitOn "-" s in (v1, v2)

keys :: [(String, String)] -> [String]
keys ss = nub (l ++ r) where (l, r) = unzip ss

mkGraph :: [(String, String)] -> GraphRepr
mkGraph ss = G.graphFromEdges [ (k, k, findConnected ss k) | k <- keys ss ]

findConnected :: [(String, String)] -> String -> [String]
findConnected [] _ = []
findConnected ((v1, v2) : ss) k | k == v1   = v2 : rest
                                | k == v2   = v1 : rest
                                | otherwise = rest
  where rest = findConnected ss k

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse input
  putStrLn "==========Part 1=========="
  print (day12A parsed)
  putStrLn "==========Part 2=========="
  print (day12B parsed)
