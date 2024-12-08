import Parser

type InputLine = (Int, [Int])

lineP :: Parser InputLine
lineP = do
  a <- int
  string ": "
  l <- int `sepBy` char ' '
  char '\n'
  return (a, reverse l)

mainParser :: Parser [InputLine]
mainParser = some lineP

solvable :: Bool -> Int -> [Int] -> Bool
solvable _ n [x] = n == x
solvable newOp n (x:l) =
  n `mod` x == 0 && solvable newOp (n `div` x) l
  || solvable newOp (n-x) l
  -- Part 2
  || newOp && n `mod` pow10 == x && solvable newOp (n `div` pow10) l
    where lenX = length $ show x
          pow10 = 10^lenX
solvable _ _ _ = error "bad input"

-- Part 1

part1 :: [InputLine] -> Int
part1 = sum . map fst . filter (uncurry (solvable False))

-- Part 2

part2 :: [InputLine] -> Int
part2 = sum . map fst . filter (uncurry (solvable True))

main :: IO ()
main = do
  Just (input, _) <- parse mainParser <$> getContents
  putStr "Part 1: "
  print $ part1 input
  putStr "Part 2: "
  print $ part2 input
