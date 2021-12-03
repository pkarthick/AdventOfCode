module Main where

import System.Environment (getEnv)
import Data.Char
import Data.Bits (Bits(bit), rotate)
import Utils.Containers.Internal.BitUtil (bitcount)
import Data.List (partition)

day :: String 
day = "3"

part :: String 
part = "2"

readTestData :: String -> IO String
readTestData kind = do
  base_path <- getEnv "HOME"
  readFile $ base_path ++ "/github/AdventOfCode/2021/testdata/" ++ day ++ "/" ++ kind ++ "_" ++ part ++ ".in"

readSampleData :: IO String
readSampleData = readTestData "sample"

readPuzzleData :: IO String
readPuzzleData = readTestData "puzzle"

readInt :: String -> Int
readInt = read

getDecimal :: [Bool] -> Int
getDecimal xs =
  let bitCount = length xs
  in sum $ map (\p -> rotate (if xs !! p then 1 else 0) (bitCount - p - 1)) [0 .. bitCount-1]

getRating :: [String] -> Int -> (Int -> Int -> Bool) -> Char -> Int
getRating [s] _ _ _ = getDecimal $ map (=='1') s
getRating xs pos retainer bit = do
  let (xs1, xs0) = partition ((== bit) . (!! pos)) xs
  getRating (if length xs1 `retainer` length xs0 then xs1 else xs0) (pos+1) retainer bit

getLifeSupportRating :: String -> Int
getLifeSupportRating s = 
  oxygen_generator_rating * co2_scrubber_rating
  where
    ls = lines s
    (xs1, xs0) = partition (\l -> head l == '1') ls
    (o2, co2) = if length xs1 >= length xs0 then (xs1, xs0) else (xs0, xs1)
    oxygen_generator_rating = getRating o2 1 (>=) '1'
    co2_scrubber_rating = getRating co2 1 (<=) '0'

main :: IO ()
main = do
  
  print . getLifeSupportRating =<< readSampleData
  print . getLifeSupportRating =<< readPuzzleData
  
