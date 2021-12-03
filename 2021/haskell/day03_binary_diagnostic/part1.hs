module Main where

import System.Environment (getEnv)
import Data.Char
import Data.Bits (Bits(bit), rotate)
import Utils.Containers.Internal.BitUtil (bitcount)

day :: String 
day = "3"

part :: String 
part = "1"

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

getPowerConsumption :: String -> Int
getPowerConsumption s = 
  gamma * epsilon
  where
    ls = lines s
    total = length ls
    bitCount = length (head ls)
    rate bit = getDecimal $ map (\p -> (\s -> s > total - s) $ sum $ map (\l -> if l !! p == bit then 1 else 0) ls) [0 .. bitCount-1]
    gamma = rate '1'
    epsilon = rate '0'

main :: IO ()
main = do
  
  print . getPowerConsumption =<< readSampleData
  print . getPowerConsumption =<< readPuzzleData
  
