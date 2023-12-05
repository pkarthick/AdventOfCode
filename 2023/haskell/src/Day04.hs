module Day04 (part1, part2) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Card = Card {number :: Int, winning :: [Int], mine :: [Int]} deriving (Show)

createCard :: String -> Card
createCard line =
  Card {number = num, winning = winningNums, mine = mineNums}
  where
    (_ : digits : xs) = words line
    num = read (init digits) :: Int
    (ws, "|" : ms) = span (/= "|") xs
    winningNums = map (read :: String -> Int) ws
    mineNums = map (read :: String -> Int) ms

matchingNumbers :: Card -> Int
matchingNumbers Card {winning = ws, mine = ms} =
  length $ filter (`elem` ms) ws

winningCount :: Card -> Int
winningCount card =
  let count = matchingNumbers card
   in if count == 0 then 0 else iterate (* 2) 1 !! (count - 1)

updateCopies :: [Card] -> Map.Map Int Int -> Int -> Map.Map Int Int
updateCopies cards m i =
  foldl
    ( \m1 n ->
        let copies = Maybe.fromMaybe 0 (Map.lookup n m)
         in Map.insert n (copies + times) m1
    )
    m
    copiesToAdd
  where
    card@Card {number = cardNo} = cards !! (i - 1)
    count = matchingNumbers card
    times = Maybe.fromMaybe 0 (Map.lookup i m)
    copiesToAdd = map (+ cardNo) [1 .. count]

part1 :: String -> IO ()
part1 input = do
  print $ sum $ map (winningCount . createCard) $ lines input

part2 :: String -> IO ()
part2 input = do
  print $ sum $ Map.map id $ foldl (updateCopies cards) initialCopies indices
  where
    cards = map createCard (lines input)
    indices = [1 .. length cards]
    initialCopies = Map.fromList [(index, 1) | index <- indices]