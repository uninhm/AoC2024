{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
import Parser

data Instruction = Mul Int Int | Do | Dont
  deriving Show

mul :: Parser Instruction
mul = do
  string "mul("
  a <- int
  char ','
  b <- int
  char ')'
  return $ Mul a b

do' = string "do()" *> pure Do
dont = string "don't()" *> pure Dont

mainParser :: Parser [Instruction]
mainParser = some $ next $ mul <|> do' <|> dont

-- Part 1

part1 :: [Instruction] -> Int
part1 (i:l) = case i of
  Mul a b -> a*b + part1 l
  _ -> part1 l
part1 [] = 0

-- Part2

part2 :: [Instruction] -> Int
part2 l = go 1 l
  where
    go enabled (i:l) = case i of
      Mul a b -> enabled*a*b + go enabled l
      Do -> go 1 l
      Dont -> go 0 l
    go _ [] = 0

solve :: ([Instruction] -> Int) -> String -> IO ()
solve part = print . fmap (part . fst) . parse mainParser

main :: IO ()
main = do
  input <- getContents
  putStr "Part 1: "
  solve part1 input
  putStr "Part 2: "
  solve part2 input
