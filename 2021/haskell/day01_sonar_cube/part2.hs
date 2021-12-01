module Main where

import System.Environment (getEnv)

day :: String 
day = "1"

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

getDepths :: String -> [Int]
getDepths = map readInt . lines 

getIncreases :: Ord a => [a] -> Int
getIncreases depths =
  length $ filter (uncurry (<)) $ zip depths (drop 1 depths)

getSlidingSum :: [Int] -> [Int]
getSlidingSum depths =
  map (\(a,b,c) -> a + b + c) $ zip3 depths (drop 1 depths) (drop 2 depths)

main :: IO ()
main = do

  print . getIncreases . getSlidingSum . getDepths =<< readSampleData
  print . getIncreases . getSlidingSum . getDepths =<< readPuzzleData
  
