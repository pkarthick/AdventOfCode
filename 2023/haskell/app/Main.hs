module Main (main) where

import System.Environment   
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04 
import qualified Day05

main :: IO ()
main = do

    args <- getArgs

    let days = 
            [
                (Day01.part1, Day01.part2),
                (Day02.part1, Day02.part2),
                (Day03.part1, Day03.part2),
                (Day04.part1, Day04.part2),
                (Day05.part1, Day05.part2)
            ]

    input <- readFile $ "../input/" ++ args !! 1

    let d = read (head args)::Int
    putStrLn $ "Day " ++ show d
    let (part1, part2) = days !! (d-1)

    part1 input
    part2 input
