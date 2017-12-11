module Main where

import System.Environment
import System.Random
import Control.Monad.State
import Data.List
import Text.Read

import Strings
import MapGeneration
import Display
import Desert

type Desert = [[Int]]
type Sdesert = [[String]]
type PlayerPos = (Int, Int)

main :: IO ()
main = do
  let ppos = (0,0)
  params <- paramsLoop
  let tileList = initTileList params
  let genList = infiniteGenerators (mkStdGen 33)
  let randomTiles = randomDesert (mkStdGen (initialSeed params)) tileList (repeat "A") params genList
  let randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
  let randomTreasuresTiles = (map . map) (corresp params) randomTreasures
  let desert = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles
  let desert' = (map . map) initDiscovered desert
  gameLoop ppos desert' params (maxWater params) 0


gameLoop :: PlayerPos -> [[(Bool, String)]] -> Params -> Int -> Int -> IO ()
gameLoop ppos desert params currentWater currentTreasures = do
  let los' = getLos ppos (los params)
  desert' <- uncoverTiles desert los'
  _ <- printInfos desert' ppos currentWater currentTreasures
  input <- getLine
  newpos <- doMove input ppos
  currentWater' <- fillOrDecrementWater currentWater desert' newpos (maxWater params)
  endGame <- checkEndGame desert' (snd newpos) currentWater'
  currentTreasures' <- checkTreasureFound currentTreasures (snd(desert' !! fst(snd newpos) !! snd (snd newpos)))

  if currentTreasures' /= currentTreasures
    then do
      desert'' <- pickUpTreasure desert' (snd newpos)
      _ <- return desert''
      checkContinue endGame newpos desert'' params currentWater' currentTreasures'

    else do
      desert'' <- returnDesert desert'
      _ <- return desert''
      checkContinue endGame newpos desert'' params currentWater' currentTreasures'


checkContinue :: Int -> (a, PlayerPos) -> [[(Bool, String)]] -> Params -> Int -> Int -> IO ()
checkContinue endGame newpos desert'' params currentWater' currentTreasures'
    | endGame == 0 =
      gameLoop (snd newpos) desert'' params currentWater'
        currentTreasures'
    | endGame == 1 = putStrLn "You're DEAD !"
    | otherwise = when (endGame == 2) $ putStrLn "You WON !"


checkEndGame :: [[(Bool, String)]] ->  PlayerPos -> Int -> IO Int
checkEndGame desert ppos currWater
  | snd(desert !! fst ppos !! snd ppos) `elem` [lavaTile, "L'"] || currWater == 0 = return 1
  | snd(desert !! fst ppos !! snd ppos) == portalTile = return 2
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
