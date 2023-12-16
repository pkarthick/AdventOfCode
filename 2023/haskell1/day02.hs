module Main where

import Data.List.Split

import Data.List (split)

data Color = 
    Red | Green | Blue deriving (Show, Eq, Ord)

newtype GameSet = GameSet [(Int, Color)] 

data Game = Game Int [GameSet]


main :: IO ()
main = do
  input <- readFile "../input/day02"
  print $ take 10 $ lines input
  
  part1 input
  part2 input

parse :: String -> Game
parse s = 
    Game ((read :: String -> Int) id) sets
    where
        ('G': 'a' : 'm': 'e' : id, ':' : rest) = span (/= ':') s
        x = split 

        sets = []

part1 :: String -> IO ()
part1 input = do


  print ""

part2 :: String -> IO ()
part2 input = do
    print ""