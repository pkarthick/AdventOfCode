module Main where

import Data.Map qualified as Map
import Data.Maybe qualified as Maybe

data Card = Card {number :: Int, winning :: [Int], mine :: [Int]} deriving (Show)

getCard :: String -> Card
getCard line =
  Card {number = number, winning = winning, mine = mine}
  where
    (_ : num : xs) = words line
    number = read (init num) :: Int
    (ws, "|" : ms) = span (/= "|") xs
    winning = map (read :: String -> Int) ws
    mine = map (read :: String -> Int) ms

winningMatches :: Card -> Int
winningMatches Card {winning = ws, mine = ms} =
  length $ filter (`elem` ms) ws

winningCount :: Card -> Int
winningCount card@Card {winning = ws, mine = ms} =
  let count = winningMatches card
   in if count == 0 then 0 else iterate (* 2) 1 !! (count - 1)

updateCopies :: [Card] -> Map.Map Int Int -> Int -> Map.Map Int Int
updateCopies cards m i =
  foldl
    ( \m n ->
        let copies = Maybe.fromMaybe 0 (Map.lookup n m)
         in Map.insert n (copies + times) m
    )
    m
    copiesToAdd
  where
    card@Card {number = cardNo} = cards !! (i - 1)
    count = winningMatches card
    times = Maybe.fromMaybe 0 (Map.lookup i m)
    copiesToAdd = map (+ cardNo) [1 .. count]

main :: IO ()
main = do
  input <- readFile "../input/day04"
  part1 input
  part2 input

part1 :: String -> IO ()
part1 input = do
  print $ sum $ map (winningCount . getCard) $ lines input

part2 :: String -> IO ()
part2 input = do
  print $ sum $ Map.map id $ foldl (updateCopies cards) initialCopies indices
  where
    cards = map getCard (lines input)
    indices = [1 .. length cards]
    initialCopies = Map.fromList [(index, 1) | index <- indices]