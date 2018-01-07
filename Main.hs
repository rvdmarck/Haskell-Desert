module Main where

import System.Environment
import System.Random
import Control.Monad.State
import Data.List
import Text.Read
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as Vec
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Control.Concurrent.STM
import Text.ParserCombinators.Parsec



import Strings
import MapGeneration
import Display
import DisplayGUI
import Desert
import Parser

type Vec        = Vec.Vector

type Desert = [[String]]
type Coordinate = (Int, Int)



main :: IO ()
main = do
  parseInfos <-  parseFromFile gameParser "save.txt"
  case parseInfos of
    Left err -> print err
    Right parseInfos -> launchGameFromFile parseInfos
  

  --gameLoop ppos desert params (maxWater params) 0 []

launchGameFromFile :: ParseInfos -> IO()
launchGameFromFile parseInfos = do
  let ppos = parsedPlayerPos parseInfos
  let params = parsedParams parseInfos
  let tileList = initTileList params
  let genList = infiniteGenerators (mkStdGen 33)
  let randomTiles = randomDesert (mkStdGen (initialSeed params)) tileList (repeat "A") params genList
  let randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
  let randomTreasuresTiles = (map . map) (corresp params) randomTreasures
  let desert = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles

  play    (InWindow "Desert Game" (windowWidth,windowHeight+100) (600,200)) 
           white 100
           (Gamestate 
              desert                                      -- desert
              params                                      -- parameters
              ppos                                        -- player position                    
              ppos                                        -- precedent step's player position
              (maxWater params)                           -- current water supply
              0                                           -- current treasures number
              (Set.fromList(getLos ppos (los params)))    -- coordinates of discovered tiles
              []                                          -- worms list
              (infiniteGenerators (mkStdGen 42))          -- infinite generators (used to spawn worms)
              0                                           -- current step
              False                                       -- boolean game is started or not
              (mkStdGen 7))
           makePicture 
           handleEvent 
           stepWorld

handleEvent :: Event -> Gamestate -> Gamestate
handleEvent event gamestate 
  | gameStarted gamestate = handleEventGameStarted event gamestate
  | otherwise = handleEventNotGameStarted event gamestate

handleEventGameStarted :: Event -> Gamestate -> Gamestate
handleEventGameStarted event gamestate 
  | EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'd') Down _ _ <- event
  = gamestate {playerPos = (fst(playerPos gamestate), snd(playerPos gamestate) + 1)}

  | EventKey (Graphics.Gloss.Interface.Pure.Game.Char 's') Down _ _ <- event
  = gamestate {playerPos = (fst(playerPos gamestate) + 1, snd(playerPos gamestate))}

  | EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'w') Down _ _ <- event
  = if fst(playerPos gamestate) > 0
      then gamestate {playerPos = (fst(playerPos gamestate) - 1, snd(playerPos gamestate))}
      else gamestate

  | EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'a') Down _ _ <- event
  = if snd(playerPos gamestate) > 0
      then gamestate {playerPos = (fst(playerPos gamestate), snd(playerPos gamestate) - 1)}
      else gamestate

  | otherwise
  = gamestate

handleEventNotGameStarted :: Event -> Gamestate -> Gamestate
handleEventNotGameStarted event gamestate 
  | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event =
      if x >= -50 && x <= 50 && y >= 75 && y <= 125
        then gamestate {gameStarted = True}
        else gamestate
  | otherwise = gamestate



stepWorld :: Float -> Gamestate -> Gamestate
stepWorld _ gamestate 
  = if oldPlayerPos gamestate /= playerPos gamestate
    then let  newLos = getLos (playerPos gamestate) (los (parameters gamestate))
              g'' = gamestate {worms = map (moveWorm gamestate) (worms gamestate)}
              g' = spawnWorms (discoveredTiles gamestate) g''
              g = g' {
                    oldPlayerPos = playerPos g'
                  , discoveredTiles = Set.union (discoveredTiles g') (Set.fromList newLos)
                  , currentWater = fillOrDecrementWater2 (currentWater g') (desert g') (True, playerPos g') (maxWater (parameters g'))
                  , currentTreasures = checkTreasureFound2 (currentTreasures g') (desert g' !! fst(playerPos g') !! snd(playerPos g'))
                  , currentStep = currentStep g' + 1}
          in if currentTreasures g' /= currentTreasures g
              then g{desert = replaceAt (playerPos g) (desert g) desertTile}
              else g
    else gamestate

randomSt :: (RandomGen g, Random a, Num a) => Control.Monad.State.State g a  
randomSt = state (randomR (0,99)) 


spawnWorms :: Set.Set Coordinate -> Gamestate ->  Gamestate
spawnWorms discoveredTiles g =
  let d = desert g  
      randomList = head (infiniteRandomLists (generators g !! currentStep g)) -- to optimize with directly 1 list
      maybeWormList = map (\(coord,proba) -> spawnWorm d (wormSpawn (parameters g)) coord proba) $ zip (reduceSpawnLocations (worms g) (Set.toList discoveredTiles)) randomList
      wormList = Maybe.catMaybes maybeWormList
      wormList' = map createWorm wormList
  in g{worms = worms g ++ wormList'}

createWorm :: Coordinate -> Worm
createWorm coord = Worm [coord] True

spawnWorm :: Desert -> Int -> Coordinate -> Int -> Maybe Coordinate
spawnWorm d spawnRate (x,y) proba = 
  if d!!x!!y == desertTile && proba < spawnRate 
    then Just (x,y)
    else Nothing

reduceSpawnLocations :: [Worm] -> [Coordinate] -> [Coordinate]
reduceSpawnLocations worms discoveredTiles = 
  let wormsCoords = map coords worms
  in foldl (\\) discoveredTiles wormsCoords



moveWorm :: Gamestate -> Worm -> Worm
moveWorm gamestate worm = 
  let d = desert gamestate
      wormHead = head (coords worm)
      adjTiles = getAdjTiles wormHead
      adjCandidates = zip adjTiles (map (\(x,y) -> d!!x!!y == "D") adjTiles)
      finalCandidates = filter snd adjCandidates
      proba = round ((1 / fromIntegral (length finalCandidates)) *100)
      randomValue = runState randomSt (generator gamestate)
      targetTile = selectWormDirection proba finalCandidates (fst randomValue) 
  in  if Maybe.isJust targetTile
      then worm { coords = Maybe.fromJust targetTile : coords worm}
      else worm
    

selectWormDirection :: Int -> [(Coordinate, Bool)] -> Int -> Maybe Coordinate
selectWormDirection proba candidates randomValue
  | proba == 0 = Nothing
  | proba == 100 = Just $ fst $ head candidates
  | otherwise = Just $ fst (candidates !! (randomValue `div` proba))

getAdjTiles :: Coordinate -> [Coordinate]
getAdjTiles (x,y)
    | x < 1 && y < 1  = [(x+1,y), (x,y+1)]
    | x < 1           = [(x+1,y), (x,y+1), (x,y-1)]
    | y < 1           = [(x+1,y), (x,y+1), (x-1,y)]
    | otherwise       = [(x+1,y), (x,y+1), (x,y-1), (x-1,y)]




gameLoop :: Coordinate -> Desert -> Params -> Int -> Int -> [Coordinate] -> IO ()
gameLoop ppos desert params currentWater currentTreasures undiscoveredTilesCoord = do
  let los' = getLos ppos (los params)
  let undiscoveredTilesCoord' = undiscoveredTilesCoord ++ los'
  _ <- printInfos desert ppos currentWater currentTreasures undiscoveredTilesCoord'
  input <- getLine
  newpos <- doMove input ppos
  currentWater' <- fillOrDecrementWater currentWater desert newpos (maxWater params)
  endGame <- checkEndGame desert (snd newpos) currentWater'
  currentTreasures' <- checkTreasureFound currentTreasures (desert !! fst(snd newpos) !! snd (snd newpos))

  if currentTreasures' /= currentTreasures
    then do
      desert' <- pickUpTreasure desert (snd newpos)
      _ <- return desert'
      checkContinue endGame newpos desert' params currentWater' currentTreasures' undiscoveredTilesCoord'

    else do
      desert' <- returnDesert desert
      _ <- return desert'
      checkContinue endGame newpos desert' params currentWater' currentTreasures' undiscoveredTilesCoord'


checkContinue :: Int -> (a, Coordinate) -> Desert -> Params -> Int -> Int -> [Coordinate] -> IO ()
checkContinue endGame newpos desert' params currentWater' currentTreasures' undiscoveredTilesCoord
    | endGame == 0 =
      gameLoop (snd newpos) desert' params currentWater'
        currentTreasures' undiscoveredTilesCoord
    | endGame == 1 = putStrLn "You're DEAD !"
    | otherwise = when (endGame == 2) $ putStrLn "You WON !"


checkEndGame :: Desert ->  Coordinate -> Int -> IO Int
checkEndGame desert ppos currWater
  | desert !! fst ppos !! snd ppos `elem` [lavaTile, "L'"] || currWater == 0 = return 1
  | desert !! fst ppos !! snd ppos == portalTile = return 2
  | otherwise = return 0











