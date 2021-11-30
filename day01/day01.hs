module Main where

parse :: String -> a
parse = undefined 

solve :: a -> b
solve = undefined

toStr :: b -> String 
toStr = undefined

main :: IO()
main = do
  input <- readFile "input.txt"
  putStr $ (toStr . solve . parse) input
  