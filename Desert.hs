module Desert
(
  getLos
, uncoverTiles
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

-- Get the position of the tiles contained in the given Line of Sight
getLos :: PlayerPos -> Int -> [PlayerPos]
getLos ppos llos =
  let xs = [-llos..llos]
    in [(x + fst ppos, y + snd ppos) | x <- xs, y <- xs, abs x + abs y <= llos, x + fst ppos >= 0 && y + snd ppos >= 0]

-- Place the discover boolean value at True at the given position
uncoverTiles :: [[(Bool, String)]] -> [(Int, Int)] -> IO [[(Bool, String)]]
uncoverTiles desert [] = return desert
uncoverTiles desert losPos =
  let desert' = replaceAt (head losPos) desert (snd(desert!!fst (head losPos)!!snd (head losPos)))
  in uncoverTiles desert' (drop 1 losPos)


doMove :: String -> PlayerPos -> IO(Bool, PlayerPos)
doMove direction ppos =
  return (runState (move direction) ppos)

fillOrDecrementWater :: Int -> [[(Bool, String)]] -> (Bool, PlayerPos) -> Int -> IO Int
fillOrDecrementWater currentWater desert newpos maxWater'
  | snd(desert !! fst (snd newpos) !! snd (snd newpos)) == waterTile =
      return maxWater'
  | not (fst newpos) = return currentWater
  | otherwise = return (currentWater - 1)

checkTreasureFound :: Int -> String -> IO Int
checkTreasureFound currentTreasures tile =
  if tile == treasureTile then return (currentTreasures + 1) else return currentTreasures

-- replace the tile at the given position
pickUpTreasure :: [[(Bool, String)]] -> PlayerPos -> IO [[(Bool, String)]]
pickUpTreasure desert ppos = do
  let d = replaceAt ppos desert desertTile
  return d

returnDesert :: [[(Bool, String)]] -> IO [[(Bool, String)]]
returnDesert = return

replaceAt :: PlayerPos -> [[(Bool, String)]] -> String -> [[(Bool, String)]]
replaceAt ppos desert val =
  let (x,_:ys) = splitAt (snd ppos) (desert !! fst ppos)
    in let (x',_ : ys') = splitAt (fst ppos) desert
      in x' ++ [x ++ (True, val) : ys] ++ ys'

move :: String -> State PlayerPos Bool
move d = state $ \(row, col) -> case d of
  "w" -> if row > 0 then (True, (row-1, col)) else (False, (row, col))
  "s" -> (True, (row+1, col))
  "d" -> (True, (row, col+1))
  "a" -> if col > 0 then (True, (row, col-1)) else (False, (row, col))
  _ -> (False, (row, col))
