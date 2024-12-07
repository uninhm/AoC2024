import Data.Array.IArray
import qualified Data.Set as S

data Direction = U | D | L | R
  deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Grid = Array Coord Char

move :: Direction -> Coord -> Coord
move U (y, x) = (y-1, x)
move D (y, x) = (y+1, x)
move L (y, x) = (y, x-1)
move R (y, x) = (y, x+1)

free :: Grid -> Coord -> Bool
free grid coord = grid ! coord /= '#'

out :: (Coord, Coord) -> Coord -> Bool
out bs (y, x) =
  y < minY || y > maxY || x < minX || x > maxX
  where ((minY, minX), (maxY, maxX)) = bs

turnRight :: Direction -> Direction
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

fromInput :: [String] -> Grid
fromInput input =
  listArray ((1,1),(lineLenght, length input)) $ concat input
  where lineLenght = length $ head input

walk :: Grid -> Direction -> Coord -> S.Set Coord -> S.Set Coord
walk grid dir pos visited
  | out (bounds grid) newPos  = newVisited
  | free grid newPos = walk grid dir newPos newVisited
  | otherwise        = walk grid (turnRight dir) pos newVisited
  where newPos = move dir pos
        newVisited = S.insert pos visited

part1 :: Coord -> Grid -> Int
part1 initialPos grid = S.size $ walk grid U initialPos S.empty

-- Part 2

isLoop :: (Coord, Coord) -> Coord -> S.Set Coord -> Bool
isLoop bs initialPos obstacles = go S.empty U initialPos
  where
    go visited dir pos
      | (dir, pos) `S.member` visited = True
      | out bs newPos  = False
      | not (newPos `S.member` obstacles) = go newVisited dir newPos
      | otherwise        = go newVisited (turnRight dir) pos
      where newPos = move dir pos
            newVisited = S.insert (dir, pos) visited

part2 :: Coord -> Grid -> Int
part2 initialPos grid =
  length $
    filter (isLoop (bounds grid) initialPos)
      [ S.insert pos obstacles
      | pos <- path, pos /= initialPos
      ]
  where obstacles = S.fromList $ map fst $ filter ((=='#') . snd) $ assocs grid
        path = S.toList (walk grid U initialPos S.empty)

main :: IO ()
main = do
  input <- lines <$> getContents
  let grid = fromInput input
      initialPos = fst $ head $ filter ((=='^') . snd) $ assocs grid
  putStr "Part 1: "
  print $ part1 initialPos grid
  putStr "Part 2: "
  print $ part2 initialPos grid
