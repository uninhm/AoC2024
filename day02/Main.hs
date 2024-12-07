isSafe :: (Num a, Ord a) => (a -> a -> Bool) -> Bool -> [a] -> Bool
isSafe ord tolerate l = go tolerate [] l
  where
    go tolerate l' (x:y:l)
      | x `ord` y && 1 <= d && d <= 3 = go tolerate (x:l') (y:l)
      | otherwise = tolerate && (go False [] (reverse l' ++ y:l)
                             ||  go False [] (reverse l' ++ x:l))
          where d = abs (x - y)
    go _ _ _ = True

calc :: Bool -> [[Int]] -> Int
calc tolerate = length . filter f
  where f l = isSafe (>) tolerate l || isSafe (<) tolerate l

-- Part 1

part1 :: [[Int]] -> Int
part1 = calc False

-- Part 2

part2 :: [[Int]] -> Int
part2 = calc True

solve :: ([[Int]] -> Int) -> String -> IO ()
solve part = print . part . map (map read . words) . lines

main :: IO ()
main = do
  input <- getContents
  putStr "Part 1: "
  solve part1 input
  putStr "Part 2: "
  solve part2 input
