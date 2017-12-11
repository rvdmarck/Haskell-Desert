module MapGeneration
(
  infiniteGenerators
, infiniteRandomLists
, randomDesert
, corresp
, compareTreasure
, initTileList
, Params (..)
)where

import System.Random
import qualified Data.List as L

import Strings

data Params = Params { los :: Int
                   , maxWater :: Int
                   , initialSeed :: Int
                   , treasurelh :: Int
                   , waterlh :: Int
                   , portallh :: Int
                   , lavalh :: Int
                   , lavalh' :: Int
                   } deriving (Show)

type Desert = [[String]]
type Coordinate = (Int, Int)


--Generate infinite list of generators
infiniteGenerators :: (RandomGen g) => g -> [g]
infiniteGenerators = L.unfoldr (Just . split)

-- Generates infinite lists of infinite Int elements
infiniteRandomLists :: (RandomGen g) => g -> [[Int]]
infiniteRandomLists = map (randomRs (0,99)) . infiniteGenerators

-- Generate a desert filled with random tiles according to the given parameters
randomDesert ::StdGen -> [String] -> [String] -> Params -> [StdGen] -> Desert
randomDesert gen tileList precTileLine params (genListHead:genListTail) =
  let currentTileLine = randomTileLine gen tileList "A" params precTileLine
    in currentTileLine : randomDesert genListHead tileList currentTileLine params genListTail

-- Generate a line of the desert
randomTileLine :: StdGen -> [String] -> String -> Params -> [String] -> [String]
randomTileLine gen tileList precTile params (x:xs) =
   let (tile, newGen) = randomTile gen tileList precTile params x
    in tile : randomTileLine newGen tileList tile params xs

--Generate a tile for the desert
randomTile :: StdGen -> [String] -> String -> Params -> String -> (String, StdGen)
randomTile gen tileList precTile params aboveTile =
  let (proba, newGen) = randomR (0,99) gen::(Int, StdGen)
    in (correspTile proba tileList precTile params newGen aboveTile, newGen)

-- Correspondance function between the likelihood parameters of a tile and the random probability
correspTile :: Int -> [String] -> String -> Params -> StdGen -> String -> String
correspTile proba tileList precTile params gen aboveTile
  | tileList !! proba /= desertTile = tileList !! proba
  | otherwise =
    let (newProba, _) = randomR (0,99) gen
    in
    if precTile == lavaTile || aboveTile == lavaTile
      then
        if newProba < lavalh' params
          then lavaTile
          else desertTile
      else
        if newProba < lavalh params
          then lavaTile
          else desertTile


-- Correspondance function between treasure tile likelihood and random probability
corresp :: Params -> Int -> String
corresp params proba
  | proba < treasurelh params = treasureTile
  | otherwise = desertTile

-- Function used to zip Desert and Treasure tiles
compareTreasure :: String -> String -> String
compareTreasure x y  =
  if x == desertTile
    then
      if y ==treasureTile
        then treasureTile
        else desertTile
    else x

-- List of tiles used to make the correspondance between a random probability and the likelihood parameters
initTileList :: Params -> [String]
initTileList params =
  let tmpList = replicate (waterlh params) waterTile ++ replicate (portallh params) portalTile
    in tmpList ++ replicate (100 - length tmpList) desertTile
