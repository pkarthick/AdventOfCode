module Day03 (part1, part2) where

import Data.Char (isDigit, ord)
import Data.List (nub)
import qualified Data.Map as M

data Position = Position Int Int deriving (Show, Eq, Ord)

data Number = Number {number :: Int, numSize :: Int, numPosition :: Position} deriving (Show, Eq)

data Symbol = Symbol {symbol :: Char, symPosition :: Position} deriving (Show, Eq)

data Schematic = Schematic Int Int (M.Map Position Number) (M.Map Position Symbol) deriving (Show)

parseLine :: Int -> [(Char, Int)] -> M.Map Position Number -> M.Map Position Symbol -> (M.Map Position Number, M.Map Position Symbol)
parseLine _ [] nums symbols = (nums, symbols)
parseLine r (('.', _) : xs) nums symbols = parseLine r xs nums symbols
parseLine r xs@((x, c) : xt) nums symbols =
  if isDigit x
    then parseLine r rest (M.insert pos (Number {number = num, numSize = length digits, numPosition = Position r c}) nums) symbols
    else parseLine r xt nums (M.insert pos (Symbol {symbol = x, symPosition = Position r c}) symbols)
  where
    (digits, rest) = span (isDigit . fst) xs
    num = foldl (\t d -> t * 10 + ord d - 48) 0 (map fst digits)
    pos = Position r c

adjacentPositions :: Int -> Int -> Number -> [(Position, Number)]
adjacentPositions height width numInfo@Number {numPosition = Position r c, numSize = size} = do
  let left = filter inBounds [(r - 1, c - 1), (r, c - 1), (r + 1, c - 1)]
  let middle = concatMap (\s -> filter inBounds [(r - 1, c + s), (r + 1, c + s)]) [0 .. size - 1]
  let right = filter inBounds [(r - 1, c + size), (r, c + size), (r + 1, c + size)]
  map (\(rr, cc) -> (Position rr cc, numInfo)) $ left ++ middle ++ right
  where
    inBounds :: (Int, Int) -> Bool
    inBounds (rr, cc) = rr >= 0 && rr < height && cc >= 0 && cc < width

numbersNearSymbols :: Schematic -> [Number]
numbersNearSymbols (Schematic height width nums symbols) = do
  let numInfoList = M.map id nums
  let symbolPositions = M.keysSet symbols
  nub $ concatMap (map snd . filter (\(pos, _) -> pos `elem` symbolPositions) . adjacentPositions height width) numInfoList

numbersNearAsterisk :: Schematic -> M.Map Position [Number]
numbersNearAsterisk (Schematic height width nums symbols) = do
  let symbolPositions = M.keysSet $ M.filter ((== '*') . symbol) symbols
  let xs = concatMap (filter (\(pos, _) -> pos `elem` symbolPositions) . adjacentPositions height width) nums
  foldl (\m (pos, num) -> M.insertWith (++) pos [num] m) M.empty xs

createSchematic :: String -> Schematic
createSchematic input = do
  Schematic height width nums symbols
  where
    ls = lines input
    height = length ls
    width = length $ head ls
    (nums, symbols) =
      foldl (\(nums1, symbols1) (r, line) -> parseLine r (zip line [0 ..]) nums1 symbols1) (M.empty, M.empty) $ zip [0 ..] ls

part1 :: String -> IO ()
part1 input = do
  let sch = createSchematic input
  let numbers = numbersNearSymbols sch
  let sumOfIds = sum $ map number numbers
  print sumOfIds

part2 :: String -> IO ()
part2 input = do
  let sch = createSchematic input
  let numbers = numbersNearAsterisk sch
  let gears = M.filter ((== 2) . length) numbers
  let sumOfGearRatio = sum $ map ((\nums -> number (head nums) * number (nums !! 1)) . snd) $ M.assocs gears
  print sumOfGearRatio
