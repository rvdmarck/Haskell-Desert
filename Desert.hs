module Desert
(
  getLos
, addUncoveredTiles
, replaceAt
, doMove
, fillOrDecrementWater
, fillOrDecrementWater2
, checkTreasureFound
, checkTreasureFound2
, pickUpTreasure
, returnDesert
, Gamestate (..)
)where

import Control.Monad.State
import qualified Data.Set as Set
import System.Random

import Strings
import MapGeneration


type Coordinate = (Int, Int)
type Desert = [[String]]

data Gamestate = Gamestate
                 { desert              :: Desert
                 , parameters          :: Params
                 , playerPos           :: Coordinate
                 , oldPlayerPos        :: Coordinate
                 , currentWater        :: Int
                 , currentTreasures    :: Int
                 , losCoords           :: [Coordinate]
                 , discoveredTiles     :: Set.Set Coordinate
                 , worms               :: [[Coordinate]]
                 , generators          :: [StdGen]
                 , currentStep         :: Int }

-- Get the position of the tiles contained in the given Line of Sight
getLos :: Coordinate -> Int -> [Coordinate]
getLos ppos llos =
  let xs = [-llos..llos]
    in [(x + fst ppos, y + snd ppos) | x <- xs, y <- xs, abs x + abs y <= llos, x + fst ppos >= 0 && y + snd ppos >= 0]

addUncoveredTiles :: [Coordinate] -> [Coordinate] -> [Coordinate]
addUncoveredTiles losCoordinates uncoveredTiles = losCoordinates ++ uncoveredTiles


doMove :: String -> Coordinate -> IO(Bool, Coordinate)
doMove direction ppos =
  return (runState (move direction) ppos)

fillOrDecrementWater :: Int -> Desert -> (Bool, Coordinate) -> Int -> IO Int
fillOrDecrementWater currentWater desert newpos maxWater'
  | desert !! fst (snd newpos) !! snd (snd newpos) == waterTile =
      return maxWater'
  | not (fst newpos) = return currentWater
  | otherwise = return (currentWater - 1)

fillOrDecrementWater2 :: Int -> Desert -> (Bool, Coordinate) -> Int -> Int
fillOrDecrementWater2 currentWater desert newpos maxWater'
  | desert !! fst (snd newpos) !! snd (snd newpos) == waterTile =
      maxWater'
  | not (fst newpos) = currentWater
  | otherwise = currentWater - 1

checkTreasureFound :: Int -> String -> IO Int
checkTreasureFound currentTreasures tile =
  if tile == treasureTile then return (currentTreasures + 1) else return currentTreasures

checkTreasureFound2 :: Int -> String -> Int
checkTreasureFound2 currentTreasures tile =
  if tile == treasureTile 
    then 
      currentTreasures + 1 

    else currentTreasures


-- replace the tile at the given position
pickUpTreasure :: Desert -> Coordinate -> IO Desert
pickUpTreasure desert ppos = do
  let d = replaceAt ppos desert desertTile
  return d

returnDesert :: Desert -> IO Desert
returnDesert = return

replaceAt :: Coordinate -> Desert -> String -> Desert
replaceAt ppos desert val =
  let (x,_:ys) = splitAt (snd ppos) (desert !! fst ppos)
    in let (x',_ : ys') = splitAt (fst ppos) desert
      in x' ++ [x ++ val : ys] ++ ys'

move :: String -> State Coordinate Bool
move d = state $ \(row, col) -> case d of
  "w" -> if row > 0 then (True, (row-1, col)) else (False, (row, col))
  "s" -> (True, (row+1, col))
  "d" -> (True, (row, col+1))
  "a" -> if col > 0 then (True, (row, col-1)) else (False, (row, col))
  _ -> (False, (row, col))
