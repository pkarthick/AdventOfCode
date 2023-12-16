module Main where

import Data.Char (ord)
import Data.List (find, isInfixOf)
import Data.Map (Map, fromList, (!))

main :: IO ()
main = do
  input <- readFile "../input/day01"
  part1 input
  part2 input

digits :: [Char]
digits = ['0' .. '9']

digitStrings :: [String]
digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitStringsReverse :: [String]
digitStringsReverse = map reverse digitStrings

digitStringsMap :: Map String Int
digitStringsMap = fromList $ zip digitStrings [1 .. 9]

digitStringsReverseMap :: Map String Int
digitStringsReverseMap = fromList $ zip digitStringsReverse [1 .. 9]

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x : xt) (y : yt) = (x == y) && startsWith xt yt

findLiteral :: [Char] -> Int
findLiteral cs@(c : ct) =
  if c `elem` digits
    then ord c - 48
    else findLiteral ct

findLiteralOrString :: [Char] -> Int
findLiteralOrString cs@(c : ct) =
  if c `elem` digits
    then ord c - 48
    else case find (`startsWith` cs) digitStrings of
      Just s -> digitStringsMap ! s
      Nothing -> findLiteralOrString ct

findLastLiteralOrString :: [Char] -> Int
findLastLiteralOrString cs@(c : ct) =
  if c `elem` digits
    then ord c - 48
    else case find (`startsWith` cs) digitStringsReverse of
      Just s -> digitStringsReverseMap ! s
      Nothing -> findLastLiteralOrString ct

findDigits :: [Char] -> Int
findDigits line =
  firstDigit * 10 + lastDigit
  where
    firstDigit = findLiteral line
    lastDigit = findLiteral $ reverse line

findDigitsLiteralOrString :: [Char] -> Int
findDigitsLiteralOrString line =
  firstDigit * 10 + lastDigit
  where
    firstDigit = findLiteralOrString line
    lastDigit = findLastLiteralOrString $ reverse line

part1 :: String -> IO ()
part1 input = do
  print $ sum $ map findDigits $ lines input

part2 :: String -> IO ()
part2 input = do
  print $ sum $ map findDigitsLiteralOrString $ lines input
