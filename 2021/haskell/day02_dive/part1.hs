module Main where

import System.Environment (getEnv)

day :: String 
day = "2"

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

processInstruction :: Num a => (a, a) -> ([Char], a) -> (a, a)
processInstruction (d, w) ("forward", dis)  = (d, w + dis)
processInstruction (d, w) ("up", dis) = (d-dis, w)
processInstruction (d, w) ("down", dis) = (d+dis, w)
processInstruction _ _ = error "Unexpected Instruction Format!"

processInstructions :: String -> Int
processInstructions = uncurry (*) . foldl processInstruction (0,0) . map ((\(f:s:_) -> (f, read s::Int)) . words) . lines

main :: IO ()
main = do
  print . processInstructions =<< readSampleData
  print . processInstructions =<< readPuzzleData
  
