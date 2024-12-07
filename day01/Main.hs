{-# LANGUAGE TupleSections #-}

import Data.List (sort, transpose)
import qualified Data.Map as M

-- Part 1

dist :: [Int] -> Int
dist [a, b] = abs (a - b)
dist _ = error "bad input"

part1 :: [[Int]] -> Int
part1 = sum . map dist . transpose . map sort

-- Part2

count :: Ord a => [a] -> M.Map a Int
count = M.fromListWith (+) . map (,1)

part2 :: [[Int]] -> Int
part2 [l, r] = sum $ map (\x -> x * M.findWithDefault 0 x m) l
  where m = count r
part2 _ = error "bad input"

-- Main

solve :: ([[Int]] -> Int) -> String -> IO ()
solve part = print . part . transpose . map (map read . words) . lines

main :: IO ()
main = do
  input <- getContents
  putStr "Part 1: "
  solve part1 input
  putStr "Part 2: "
  solve part2 input
