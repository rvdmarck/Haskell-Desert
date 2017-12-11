module Desert
(
  getLos
, addUncoveredTiles
, replaceAt
, doMove
, fillOrDecrementWater
, checkTreasureFound
, pickUpTreasure
, returnDesert
)where

import Control.Monad.State
import Strings


type PlayerPos = (Int, Int)
type Coordinate = (Int, Int)
type Desert = [[String]]

-- Get the position of the tiles contained in the given Line of Sight
getLos :: PlayerPos -> Int -> [(Int, Int)]
getLos ppos llos =
  let xs = [-llos..llos]
    in [(x + fst ppos, y + snd ppos) | x <- xs, y <- xs, abs x + abs y <= llos, x + fst ppos >= 0 && y + snd ppos >= 0]

addUncoveredTiles :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
addUncoveredTiles losCoordinates uncoveredTiles = losCoordinates ++ uncoveredTiles


doMove :: String -> PlayerPos -> IO(Bool, PlayerPos)
doMove direction ppos =
  return (runState (move direction) ppos)

fillOrDecrementWater :: Int -> Desert -> (Bool, PlayerPos) -> Int -> IO Int
fillOrDecrementWater currentWater desert newpos maxWater'
  | desert !! fst (snd newpos) !! snd (snd newpos) == waterTile =
      return maxWater'
  | not (fst newpos) = return currentWater
  | otherwise = return (currentWater - 1)

checkTreasureFound :: Int -> String -> IO Int
checkTreasureFound currentTreasures tile =
  if tile == treasureTile then return (currentTreasures + 1) else return currentTreasures

-- replace the tile at the given position
pickUpTreasure :: Desert -> PlayerPos -> IO Desert
pickUpTreasure desert ppos = do
  let d = replaceAt ppos desert desertTile
  return d

returnDesert :: Desert -> IO Desert
returnDesert = return

replaceAt :: PlayerPos -> Desert -> String -> Desert
replaceAt ppos desert val =
  let (x,_:ys) = splitAt (snd ppos) (desert !! fst ppos)
    in let (x',_ : ys') = splitAt (fst ppos) desert
      in x' ++ [x ++ val : ys] ++ ys'

move :: String -> State PlayerPos Bool
move d = state $ \(row, col) -> case d of
  "w" -> if row > 0 then (True, (row-1, col)) else (False, (row, col))
  "s" -> (True, (row+1, col))
  "d" -> (True, (row, col+1))
  "a" -> if col > 0 then (True, (row, col-1)) else (False, (row, col))
  _ -> (False, (row, col))
