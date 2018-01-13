module Worm
(
    spawnWorms
    , createWorm
    , moveWorm
    , isValidTile
    , selectWormDirection
    , isDeadTM
    , isDead
    , spawnWorm
    , reduceSpawnLocations
    , getAdjTiles
)where

import qualified Control.Concurrent.STM as STM
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.List

import Desert
import Strings
import DisplayGUI
import MapGeneration

type Coordinate = (Int, Int)
type Desert = [[String]]


spawnWorms :: Set.Set Coordinate -> Gamestate -> IO Gamestate
spawnWorms discoveredTiles g = 
  let d = desert g  
      randomList = head (infiniteRandomLists (generators g !! currentStep g))
      maybeWormList = map (\(coord,proba) -> spawnWorm d (wormSpawn (parameters g)) coord proba) $ zip (reduceSpawnLocations (worms g) (Set.toList discoveredTiles)) randomList
      wormList = Maybe.catMaybes maybeWormList
  in do 
    tmvars <- mapM createWorm wormList
    return g{wormsTVars = wormsTVars g ++ tmvars}

createWorm :: Coordinate -> IO (STM.TVar Worm)
createWorm coord = STM.newTVarIO (Worm [coord] True)

moveWorm :: Gamestate -> (STM.TVar Worm, Int) -> IO ()
moveWorm gamestate (worm, random) = do
  currentWorm <- STM.atomically $ STM.readTVar worm
  if length (coords currentWorm) == wormLength (parameters gamestate) || not (isEmerging currentWorm)
  then 
    STM.atomically ( STM.writeTVar worm (currentWorm {coords = take (length (coords currentWorm)-1) (coords currentWorm), isEmerging = False}))
    
  else do
    otherWorms <-  mapM (STM.atomically . STM.readTVar) (wormsTVars gamestate)
    let d = desert gamestate
        wormHead = head (coords currentWorm)
        adjTiles = getAdjTiles wormHead
    tilesWithWorms <- mapM (\(x,y) -> coordElemMatTM (x,y) (wormsTVars gamestate)) adjTiles
    let validTiles = map (\(x,y) -> d!!x!!y == "D") adjTiles
        tilesWithNoWorm = map not tilesWithWorms
        intermediateFinalCandidates = zip3 adjTiles validTiles tilesWithNoWorm
        finalCandidates = filter isValidTile intermediateFinalCandidates
        proba = ceiling ((1 / fromIntegral (length finalCandidates)) * 100)
        targetTile = selectWormDirection proba finalCandidates random
    if Maybe.isJust targetTile
      then STM.atomically ( STM.writeTVar worm  (currentWorm {coords = Maybe.fromJust targetTile : coords currentWorm}))
      else STM.atomically ( STM.writeTVar worm  (currentWorm {coords = take (length (coords currentWorm)-1) (coords currentWorm), isEmerging = False}))
            
            
isValidTile :: (Coordinate, Bool, Bool) -> Bool
isValidTile (c, a, b) = a && b 

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

selectWormDirection :: Int -> [(Coordinate, Bool, Bool)] -> Int -> Maybe Coordinate
selectWormDirection proba candidates randomValue
  | proba == 0 = Nothing
  | proba == 100 = Just $ fst3  $ head candidates
  | proba < 100 = Just $ fst3  (candidates !! (randomValue `div` proba))
  | otherwise = Nothing

  
isDeadTM :: STM.TVar Worm -> IO Bool
isDeadTM w = do
   worm <- STM.atomically $ STM.readTVar w
   return (null (coords worm))

isDead :: Worm -> Bool
isDead w = null (coords w)


spawnWorm :: Desert -> Int -> Coordinate -> Int -> Maybe Coordinate
spawnWorm d spawnRate (x,y) proba = 
  if d!!x!!y == desertTile && proba < spawnRate 
    then Just (x,y)
    else Nothing

reduceSpawnLocations :: [Worm] -> [Coordinate] -> [Coordinate]
reduceSpawnLocations worms discoveredTiles = 
  let wormsCoords = map coords worms
  in foldl (\\) discoveredTiles wormsCoords

getAdjTiles :: Coordinate -> [Coordinate]
getAdjTiles (x,y)
    | x < 1 && y < 1  = [(x+1,y), (x,y+1)]
    | x < 1           = [(x+1,y), (x,y+1), (x,y-1)]
    | y < 1           = [(x+1,y), (x,y+1), (x-1,y)]
    | otherwise       = [(x+1,y), (x,y+1), (x,y-1), (x-1,y)]