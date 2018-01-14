module Main where

import System.Environment
import System.Random
import Control.Monad.State
import Control.Monad
import Data.List
import Text.Read
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G
import qualified Data.Vector as Vec
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async
import Text.ParserCombinators.Parsec

import Strings
import MapGeneration
import DisplayGUI
import Desert
import Parser
import Worm
import EventHandlers


type Desert = [[String]]
type Coordinate = (Int, Int)

  
main :: IO()
main =
  let params = Params 0 0 0 0 0 0 0 0 0 0
    in playIO  (InWindow "Desert Game" (windowWidth,windowHeight+100) (600,200)) 
          white 100
          (Gamestate 
              [[]]                                        -- desert
              params                                      -- parameters
              (0,0)                                       -- player position                    
              (0,0)                                       -- precedent step's player position
              (maxWater params)                           -- current water supply
              Set.empty                                   -- coordinates of discovered tiles
              []                                          -- coordinates of collected treasures
              []                                          -- worms list
              []                                          -- infinite generators (used to spawn worms)
              0                                           -- current step
              (mkStdGen 7)                                -- single generator 
              []
              0
              ""
              (GameFlags False False False False False False) )
          makePicture 
          handleEvent 
          stepWorld




                    
stepWorld :: Float -> Gamestate -> IO Gamestate
stepWorld _ gamestate = 
  if oldPlayerPos gamestate /= playerPos gamestate
    then do       
      let   newLos = getLos (playerPos gamestate) (los (parameters gamestate))
            (g1, randoms) = generateRandoms gamestate (length (worms gamestate))

      Async.mapConcurrently (moveWorm g1) (zip (wormsTVars g1) randoms)
      wormsFromTVars <- mapM (STM.atomically . STM.readTVar) (wormsTVars g1)
      filteredWormsTVars <- filterM (fmap not . isDeadTM) (wormsTVars g1)
      let   g2 = g1 {worms = filter (not . isDead) wormsFromTVars
                    ,wormsTVars = filteredWormsTVars}
            g3 = g2 {
                  oldPlayerPos = playerPos g2
                  , discoveredTiles = Set.union (discoveredTiles g2) (Set.fromList newLos)
                  , currentWater = fillOrDecrementWater2 (currentWater g2) (desert g2) (True, playerPos g2) (maxWater (parameters g2))
                  , currentStep = currentStep g2 + 1}
      g4 <- spawnWorms (Set.union (discoveredTiles gamestate) (Set.fromList (getLos (playerPos gamestate) ((los (parameters gamestate))+5)))) g3
      let endGame = checkEndGame g4
      if endGame == 1
        then return gamestate{flags = (flags gamestate) {playerDead = True, gameFinished = True}}
        else if endGame == 2
          then return gamestate{flags = (flags gamestate) {playerDead = False, gameFinished = True}}
          else
            if desert g4 !! fst(playerPos g4) !! snd(playerPos g4) == "T"
            then return g4{
                    desert = replaceAt (playerPos g4) (desert g4) desertTile
                  , collectedTreasures = playerPos g4 : collectedTreasures g4}
            else return g4
    else return gamestate


randomSt :: (RandomGen g, Random a, Num a) => Control.Monad.State.State g a  
randomSt = state (randomR (0,99)) 


generateRandoms :: Gamestate -> Int -> (Gamestate, [Int])
generateRandoms gamestate 0 = (gamestate, [])
generateRandoms gamestate n = 
  let (val, gen) = runState randomSt (generator gamestate)
      g = gamestate {generator = gen}
  in (g, val : snd (generateRandoms g (n-1)))


checkEndGame :: Gamestate -> Int
checkEndGame gamestate 
  | desert gamestate !! fst (playerPos gamestate) !! snd (playerPos gamestate) `elem` [lavaTile, "L'"] 
    || currentWater gamestate == 0 
    || coordElemMat (playerPos gamestate) (worms gamestate) = 1
  | desert gamestate !! fst (playerPos gamestate) !! snd (playerPos gamestate) == portalTile = 2
  | otherwise = 0