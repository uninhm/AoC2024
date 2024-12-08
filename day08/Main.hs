{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)

type Coord = (Int, Int)
type Grid = M.Map Char [Coord]
type Bounds = (Coord, Coord)

fromInput :: [String] -> (Bounds, Grid)
fromInput input = (bounds, m)
  where
    bounds = ((1,1), (length input, length (head input)))
    f y = zip (map (y,) [1..])
    indexed = concat $ zipWith f [1..] input
    m = M.fromListWith (++) $ map (fmap (:[]) . swap) indexed

inBound :: Bounds -> Coord -> Bool
inBound bs (y, x) =
  minY <= y && y <= maxY && minX <= x && x <= maxX
  where ((minY, minX), (maxY, maxX)) = bs

antinodes1 :: (Coord, Coord) -> [Coord]
antinodes1 ((y,x), (y',x')) =
  [(y-dy,x-dx), (y'+dy,x'+dx)]
  where dx = x' - x
        dy = y' - y

solve :: ((Coord, Coord) -> [Coord]) -> Bounds -> Grid -> Int
solve antinodeGen bounds grid =
  length $ nubOrd
         $ filter (inBound bounds)
         $ concatMap antinodeGen
         $ concat pairs
  where pairs = [ [ (a, b) | a <- l, b <- l, a /= b ]
                | (k, l) <- M.assocs grid, k /= '.'
                ]

part1 :: Bounds -> Grid -> Int
part1 = solve antinodes1

-- Part 2

antinodes2 :: (Coord, Coord) -> [Coord]
antinodes2 ((y,x), (y',x')) =
  [(y + i*dy, x + i*dx) | i <- [-50 .. 50]]
  where dx = x' - x
        dy = y' - y

part2 :: Bounds -> Grid -> Int
part2 = solve antinodes2

main :: IO ()
main = do
  input <- lines <$> getContents
  let (bounds, grid) = fromInput input
  putStr "Part 1: "
  print $ part1 bounds grid
  putStr "Part 2: "
  print $ part2 bounds grid
