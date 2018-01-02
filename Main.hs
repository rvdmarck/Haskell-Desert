module Main where

import System.Environment
import System.Random
import Control.Monad.State
import Data.List
import Text.Read
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector    as Vec


import Strings
import MapGeneration
import Display
import DisplayGUI
import Desert

type Vec        = Vec.Vector

type Desert = [[String]]
type Coordinate = (Int, Int)


main :: IO ()
main = do
  let ppos = (0,0)
  let params = Params 1 50 12 10 25 15 15 20
  --params <- paramsLoop
  let tileList = initTileList params
  let genList = infiniteGenerators (mkStdGen 33)
  let randomTiles = randomDesert (mkStdGen (initialSeed params)) tileList (repeat "A") params genList
  let randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
  let randomTreasuresTiles = (map . map) (corresp params) randomTreasures
  let desert = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles
  play    (InWindow "Desert Game" (windowWidth,windowHeight+100) (600,200)) 
           white 100
           (Gamestate desert params ppos ppos (maxWater params) 0 0 (getLos ppos (los params)) (getLos ppos (los params))) 
           makePicture 
           handleEvent 
           stepWorld

  --gameLoop ppos desert params (maxWater params) 0 []


handleEvent :: Event -> Gamestate -> Gamestate
handleEvent event gamestate 
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


stepWorld :: Float -> Gamestate -> Gamestate
stepWorld _ gamestate 
  = if oldPlayerPos gamestate /= playerPos gamestate
    then let newLos = getLos (playerPos gamestate) (los (parameters gamestate) )
         in let g = gamestate {oldPlayerPos = playerPos gamestate
                  , losCoords = newLos
                  , discoveredTiles = discoveredTiles gamestate ++ newLos
                  , currentWater = fillOrDecrementWater2 (currentWater gamestate) (desert gamestate) (True, playerPos gamestate) (maxWater (parameters gamestate))
                  , currentTreasures = checkTreasureFound2 (currentTreasures gamestate) (desert gamestate !! fst(playerPos gamestate) !! snd(playerPos gamestate))}
            in if currentTreasures gamestate /= currentTreasures g
                then g{desert = replaceAt (playerPos g) (desert g) desertTile}
                else g
    else gamestate





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

paramsLoop :: IO Params
paramsLoop = do
  putStrLn "Enter parameter s (line of sight of explorer) :"
  s <- getLine
  putStrLn "Enter parameter m (maximum measures of water) :"
  m <- getLine
  putStrLn "Enter parameter g (initial seed) :"
  g <- getLine
  putStrLn "Enter parameter t (likelihood (in %) of treasure in a desert tile) :"
  t <- getLine
  putStrLn "Enter parameter w (likelihood (in %) of water tile generation) :"
  w <- getLine
  putStrLn "Enter parameter p (likelihood (in %) of portal tile generation) :"
  p <- getLine
  putStrLn "Enter parameter l (likelihood (in %) of lava tile generation when none of the previously-generated adjacent tiles is lava) :"
  l <- getLine
  putStrLn "Enter parameter ll (likelihood (in %) of lava tile generation when at least one of the previously-generated adjacent tiles is lava) :"
  ll <- getLine
  let paramList = [s, m, g, t, w, p, l, ll]
  let maybeParamList = map readMaybe paramList ::[Maybe Int]
  if Nothing `elem` maybeParamList
    then do
      putStrLn "\n ==== \nWrong parameter input"
      paramsLoop
    else
      if read w + read p + read l > 100 || read w + read p + read ll > 100
        then do
          putStrLn "\n ==== \nWrong parameter values"
          paramsLoop
        else do
          let params' = Params (read s) (read m) (read g) (read t) (read w) (read p) (read l) (read ll)
          print params'
          return params'











