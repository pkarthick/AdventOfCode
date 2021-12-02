module Main where

import System.Environment (getEnv)

day :: String 
day = "2"

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

processInstruction :: Num a => (a, a, a) -> ([Char], a) -> (a, a, a)
processInstruction (d, w, a) ("forward", dis)  = (d + a * dis, w + dis, a)
processInstruction (d, w, a) ("up", dis) = (d, w, a - dis)
processInstruction (d, w, a) ("down", dis) = (d, w, a + dis)
processInstruction _ _ = error "Unexpected Instruction Format!"

processInstructions :: String -> Int
processInstructions = (\(d,w, _) -> d * w) . foldl processInstruction (0,0,0) . map ((\(f:s:_) -> (f, read s::Int)) . words) . lines

main :: IO ()
main = do
  print . processInstructions =<< readSampleData
  print . processInstructions =<< readPuzzleData
  
