import Parser
import qualified Data.Set as S
import Data.List (sortBy)

type Rule = (Int, Int)
type Update = [Int]

ruleP :: Parser Rule
ruleP = do
  a <- int
  char '|'
  b <- int
  char '\n'
  return (a, b)

updateP :: Parser Update
updateP = (int `sepBy` char ',') <* char '\n'

mainParser :: Parser ([Rule], [Update])
mainParser = do
  rules <- some ruleP
  char '\n'
  updates <- some updateP
  return (rules, updates)

goodOrder :: [Rule] -> Update -> Bool
goodOrder rules (x:l)
  | any (`elem` leftOfX) l = False
  | otherwise = goodOrder rules l
    where leftOfX = map fst $ filter ((==x) . snd) rules
goodOrder _ _ = True

middle :: [a] -> a
middle l = l !! (length l `div` 2)

part1 :: [Rule] -> [Update] -> IO ()
part1 rules updates =
  print $ sum $ map middle $ filter (goodOrder rules) updates

-- Part 2

setOrd :: Ord a => S.Set (a, a) -> a -> a -> Ordering
setOrd s a b
  | (a, b) `S.member` s = LT
  | otherwise = GT

part2 :: [Rule] -> [Update] -> IO ()
part2 rules updates =
  print . sum
        . map (middle . sortBy (setOrd s))
        $ filter (not . goodOrder rules) updates
  where s = S.fromList rules

main :: IO ()
main = do
  input <- getContents
  let Just ((rules, updates), _) = parse mainParser input
  putStr "Part 1: "
  part1 rules updates
  putStr "Part 2: "
  part2 rules updates
