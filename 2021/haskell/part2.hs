module Main where

readInt :: String -> Int
readInt = read

getDepths :: [Char] -> [Int]
getDepths = map readInt . lines 

getSlidingSum :: [Int] -> [Int]
getSlidingSum depths =
  map (\(a,b,c) -> a + b + c) $ zip3 depths (drop 1 depths) (drop 2 depths)

getIncreases :: [Int] -> Int
getIncreases depths =
  length $ filter (uncurry (<)) $ zip depths (drop 1 depths)

main :: IO ()
main = do
  print . getIncreases . getSlidingSum . getDepths =<< readFile "../testdata/1/sample_2.in"
  print . getIncreases . getSlidingSum . getDepths  =<< readFile "../testdata/1/puzzle_2.in"
