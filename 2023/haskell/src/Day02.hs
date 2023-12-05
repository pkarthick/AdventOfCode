module Day02 (part1, part2) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

newtype GameSet = GameSet [(Int, Color)] deriving (Show)

data Game = Game Int [GameSet] deriving (Show)

getColor :: String -> Color
getColor "green" = Green
getColor "red" = Red
getColor "blue" = Blue
getColor _ = error "Not a valid color!"

createMap :: Game -> M.Map Color Int
createMap (Game _ sets) =
  foldl (\m (GameSet xs) -> foldl (\m1 (count, color) -> M.insertWith max color count m1) m xs) M.empty sets

notFewer :: (Int, Int, Int) -> M.Map Color Int -> Bool
notFewer (red, green, blue) m = do
  foldl
    ( \ok (color, threshold) -> case M.lookup color m of
        Just r -> ok && r <= threshold
        Nothing -> False
    )
    True
    [(Red, red), (Green, green), (Blue, blue)]

powerOfTheSets :: M.Map Color Int -> Int
powerOfTheSets m =
  product $ map (\color -> fromMaybe 0 (M.lookup color m)) [Red, Green, Blue]

createGame :: String -> Game
createGame s = do
  Game gameId sets
  where
    (gameIdStr, rest) = case span (/= ':') s of
      ('G' : 'a' : 'm' : 'e' : ' ' : gameIdStr1, ':' : ' ' : rest1) -> (gameIdStr1, rest1)
      _ -> error "Unexpected input format!"
    gameId = (read :: String -> Int) gameIdStr
    xs = splitOn "; " rest
    sets = map (GameSet . map ((\ws -> ((read :: String -> Int) (head ws), getColor (ws !! 1))) . words) . splitOn ", ") xs

part1 :: String -> IO ()
part1 input = do
  let games = map createGame $ lines input
  print $ sum $ map (\(Game gameId _) -> gameId) $ filter (notFewer (12, 13, 14) . createMap) games

part2 :: String -> IO ()
part2 input = do
  let games = map createGame $ lines input
  print $ sum $ map (powerOfTheSets . createMap) games