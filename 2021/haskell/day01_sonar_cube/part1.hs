module Main where

readInt :: String -> Int
readInt = read

getDepths :: [Char] -> [Int]
getDepths = map readInt . lines 

getIncreases :: [Int] -> Int
getIncreases depths =
  length $ filter (uncurry (<)) $ zip depths (drop 1 depths)

main :: IO ()
main = do
  print . getIncreases . getDepths =<< readFile "../testdata/1/sample_1.in"
  print . getIncreases . getDepths  =<< readFile "../testdata/1/puzzle_1.in"
