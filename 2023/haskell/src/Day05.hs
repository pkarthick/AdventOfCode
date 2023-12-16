module Day05 (part1, part2) where

import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set.Internal as S

data Kind
  = Seed
  | Soil
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location
  deriving (Show, Eq, Ord)

data MapperEntry = MapperEntry {dest :: Int, src :: Int, len :: Int} deriving (Show, Eq, Ord)

-- data MapperSection = MapperSection {source :: Kind, destination :: Kind, entries :: [MapperEntry]} deriving (Show)

newtype SeedNumber = SeedNumber Int deriving (Show)

data Almanac = Almanac [Int] (M.Map (Kind, Kind) [MapperEntry]) deriving (Show)

getKind :: String -> Kind
getKind "seed" = Seed
getKind "soil" = Soil
getKind "fertilizer" = Fertilizer
getKind "water" = Water
getKind "light" = Light
getKind "temperature" = Temperature
getKind "humidity" = Humidity
getKind "location" = Location
getKind _ = error "Unexpected Kind"

createSection :: [String] -> Kind -> Kind -> M.Map (Kind, Kind) [MapperEntry] -> M.Map (Kind, Kind) [MapperEntry]
createSection es source destination
  = M.insert (source, destination) entries
  where
    -- map

    entries =
      map
        ( ( \ws ->
              MapperEntry
                { dest = (read :: String -> Int) $ head ws,
                  src = (read :: String -> Int) $ ws !! 1,
                  len = (read :: String -> Int) $ ws !! 2
                }
          )
            . words
        )
        es

createAlmanac :: String -> Almanac
createAlmanac input =
  Almanac seeds map7
  where
    m = M.empty
    seedsStr : _ : rest = lines input
    _ : seedsTail = words seedsStr
    seeds = map (read :: String -> Int) seedsTail
    (_ : seeds2Soil, _ : rest1) = span (/= "") rest
    map1 = createSection seeds2Soil Seed Soil m
    (_ : soil2Fertilizer, _ : rest2) = span (/= "") rest1
    map2 = createSection soil2Fertilizer Soil Fertilizer map1
    (_ : fertilizer2Water, _ : rest3) = span (/= "") rest2
    map3 = createSection fertilizer2Water Fertilizer Water map2
    (_ : water2Light, _ : rest4) = span (/= "") rest3
    map4 = createSection water2Light Water Light map3
    (_ : light2Temparature, _ : rest5) = span (/= "") rest4
    map5 = createSection light2Temparature Light Temperature map4
    (_ : temperature2Humidity, _ : rest6) = span (/= "") rest5
    map6 = createSection temperature2Humidity Temperature Humidity map5
    (_ : humidity2Location, _) = span (/= "") rest6
    map7 = createSection humidity2Location Humidity Location map6

nextKind :: Kind -> Maybe Kind
nextKind Seed = Just Soil
nextKind Soil = Just Fertilizer
nextKind Fertilizer = Just Water
nextKind Water = Just Light
nextKind Light = Just Temperature
nextKind Temperature = Just Humidity
nextKind Humidity = Just Location
nextKind Location = Nothing

hasMapping :: MapperEntry -> Int -> Maybe Int
hasMapping MapperEntry {dest = d, src = s, len = l} value =
  if value < s + l && value >= s
    then Just $ d + (value - s)
    else Nothing

findLocationBySeed :: M.Map (Kind, Kind) [MapperEntry] -> Kind -> Int -> Int
findLocationBySeed m sk s = do
  case nextKind sk of
    Just dk ->
      let entries = m M.! (sk, dk)
       in case dropWhile (== Nothing) $ map (`hasMapping` s) entries of
            Just d : _ -> findLocationBySeed m dk d
            _ -> findLocationBySeed m dk s
    Nothing ->
      s

hasMappingRange1 :: MapperEntry -> (Int, Int) -> [(Int, Int)]
hasMappingRange1 entry@MapperEntry {dest = d, src = s, len = l} (v, vl) =
  if v + vl <= s || v >= s + l
    then []
    else case (v < s, v + vl > s + l) of
      (True, True) -> [(v, s - v), (d, l), (s + l, v + vl - (s + l))]
      (True, False) -> [(v, s - v), (d, v + vl - s)]
      (False, True) -> [(d + (v - s), s + l - v), (s + l, (v + vl) - (s + l))]
      (False, False) -> [(d + (v - s), vl)]

hasMappingRange :: Kind -> MapperEntry -> (Int, Int) -> [(Int, Int)]
hasMappingRange _ entry@MapperEntry {dest = d, src = s, len = l} (v, vl) =
  if v + vl <= s || v >= s + l
    then []
    else 

      case (v < s, v + vl > s + l) of
        (True, True) -> [(v, s - v), (d, l), (s + l, v + vl - (s + l))]
        (True, False) -> [(v, s - v), (d, v + vl - s)]
        (False, True) -> [(d + (v - s), s + l - v), (s + l, (v + vl) - (s + l))]
        (False, False) -> [(d + (v - s), vl)]

findLocationBySeedRange :: M.Map (Kind, Kind) [MapperEntry] -> Kind -> [(Int, Int)] -> IO Int

findLocationBySeedRange m sk rs = do
  case nextKind sk of
    Just dk -> do
      case concatMap (filter (not . all null) . (\entry -> map (hasMappingRange sk entry) rs)) (m M.! (sk, dk)) of
        [] -> findLocationBySeedRange m dk rs
        rss ->
          do
            
            return
            $ minimum
            $ mapM
              ( \rs1 ->
                  findLocationBySeedRange m dk $
                    concatMap (\(pair, old) -> if null pair then old else pair) $
                      zip rs1 $
                        map (: []) rs
              )
            $ concat rss


    Nothing ->
      return $ fst $ minimum rs
findLocationBySeedRange _ _ _ = error "Chunks invalid!"

part1 :: String -> IO ()
part1 input = do
  let almanac@(Almanac seeds m) = createAlmanac input
  print $ minimum $ map (findLocationBySeed m Seed) seeds

part2 :: String -> IO ()
part2 input = do
  let almanac@(Almanac seeds m) = createAlmanac input
  let rs = map (\xs -> (head xs, xs !! 1 - 1)) $ chunksOf 2 seeds

  res <- findLocationBySeedRange m Seed rs

  print res
