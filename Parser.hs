{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}

module Parser
  ( module Parser
  , module Control.Applicative
  ) where

import Control.Applicative (Applicative, many, some, Alternative, (<|>), empty, optional)
import Data.Maybe (fromJust)
import Control.Monad (replicateM)

newtype Parser a = Parser (String -> Maybe (a, String))

parse (Parser p) = p

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap g . p)
    where g (x, s') = (f x, s')

instance Applicative Parser where
  pure x = Parser $ Just . (x,)
  (Parser p) <*> q = Parser $ \s -> p s >>= g
    where g (f, s') = parse (f <$> q) s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p) <|> (Parser q) = Parser $ \s -> p s <|> q s

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> p s >>= g
    where g (x, s') = parse (f x) s'

anyChar :: Parser Char
anyChar = Parser f
  where
    f (c:s') = Just (c, s')
    f _ = Nothing

char :: Char -> Parser Char
char c = Parser f
  where
    f (c':s')
      | c' == c = Just (c, s')
    f _ = Nothing

string :: String -> Parser String
string (c:s) = (:) <$> char c <*> string s
string _ = pure ""

digit :: Parser Char
digit = foldl1 (<|>) $ map char "0123456789"

digits :: Parser String
digits = some digit

int :: Parser Int
int = read <$> digits

times :: Applicative f => Int -> f a -> f [a]
times = replicateM

atMost :: Alternative f => Int -> f a -> f [a]
atMost 0 _ = pure []
atMost n p = ((:) <$> p <*> atMost (n-1) p) <|> pure []

next :: Parser a -> Parser a
next p = p <|> (anyChar *> next p)

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` q = (:) <$> p <*> some (q *> p)
