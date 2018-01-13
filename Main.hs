module Main where

import System.Environment
import System.Random
import Control.Monad.State
import Control.Monad
import Data.List
import Text.Read
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Vector as Vec
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async
import Text.ParserCombinators.Parsec

import Strings
import MapGeneration
import Display
import DisplayGUI
import Desert
import Parser


type Desert = [[String]]
type Coordinate = (Int, Int)


main :: IO ()
main = do
  parseInfos <-  parseFromFile gameParser "saves/test.txt"
  case parseInfos of
    Left err -> print err
    Right parseInfos -> launchGameFromFile parseInfos
  

launchGameFromFile :: ParseInfos -> IO()
launchGameFromFile parseInfos = do
  let ppos = parsedPlayerPos parseInfos
      params = parsedParams parseInfos
      wormList = parsedWorms parseInfos
      tileList = initTileList params
      genList = infiniteGenerators (mkStdGen 33)
      randomTiles = randomDesert (mkStdGen (initialSeed params)) tileList (repeat "A") params genList
      randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
      randomTreasuresTiles = (map . map) (corresp params) randomTreasures
      desert' = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles
      discoveredTiles = parsedRevealed parseInfos
      collectedTreasures = parsedCollected parseInfos
      desert = replaceAts collectedTreasures desert' "D"
  var <- Control.Monad.replicateM 3 (STM.newTVarIO (Worm [] True))
  playIO  (InWindow "Desert Game" (windowWidth,windowHeight+100) (600,200)) 
          white 100
          (Gamestate 
              desert                                      -- desert
              params                                      -- parameters
              ppos                                        -- player position                    
              ppos                                        -- precedent step's player position
              (maxWater params)                           -- current water supply
              (Set.fromList discoveredTiles)              -- coordinates of discovered tiles
              collectedTreasures                          -- coordinates of collected treasures
              wormList                                    -- worms list
              (infiniteGenerators (mkStdGen 42))          -- infinite generators (used to spawn worms)
              0                                           -- current step
              False                                       -- boolean game is started or not
              (mkStdGen 7)
              "saves/save.txt"
              var)
          makePicture 
          handleEvent 
          stepWorld

handleEvent :: Event -> Gamestate -> IO Gamestate
handleEvent event gamestate 
  | gameStarted gamestate = handleEventGameStarted event gamestate
  | otherwise = handleEventNotGameStarted event gamestate

handleEventGameStarted :: Event -> Gamestate -> IO Gamestate
handleEventGameStarted event gamestate 
  | EventKey (Graphics.Gloss.Interface.IO.Game.Char 'd') Down _ _ <- event
  = return gamestate {playerPos = (fst(playerPos gamestate), snd(playerPos gamestate) + 1)}

  | EventKey (Graphics.Gloss.Interface.IO.Game.Char 's') Down _ _ <- event
  = return gamestate {playerPos = (fst(playerPos gamestate) + 1, snd(playerPos gamestate))}

  | EventKey (Graphics.Gloss.Interface.IO.Game.Char 'w') Down _ _ <- event
  = if fst(playerPos gamestate) > 0
      then return gamestate {playerPos = (fst(playerPos gamestate) - 1, snd(playerPos gamestate))}
      else return gamestate

  | EventKey (Graphics.Gloss.Interface.IO.Game.Char 'a') Down _ _ <- event
  = if snd(playerPos gamestate) > 0
      then return gamestate {playerPos = (fst(playerPos gamestate), snd(playerPos gamestate) - 1)}
      else return gamestate

  | EventKey (SpecialKey KeyF5) Down _ _ <- event
  = let s = gameToString gamestate
    in do
      writeFile "saves/test.txt" s
      return gamestate 

  | otherwise
  = return gamestate

handleEventNotGameStarted :: Event -> Gamestate -> IO Gamestate
handleEventNotGameStarted event gamestate 
  | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event =
      if x >= -50 && x <= 50 && y >= 75 && y <= 125
        then return gamestate {gameStarted = True}
        else return gamestate
  | otherwise = return gamestate


stepWorld :: Float -> Gamestate -> IO Gamestate
stepWorld _ gamestate = 
  if oldPlayerPos gamestate /= playerPos gamestate
    then do 
      let (g1, randoms) = generateRandoms gamestate (length (worms gamestate))
      test <- Async.mapConcurrently (moveWormTM g1) (zip (wormsTVars g1) randoms)
      let   newLos = getLos (playerPos gamestate) (los (parameters gamestate))
            (g1, randoms) = generateRandoms gamestate (length (worms gamestate))
            g2 = g1 {worms = filter (not . isDead) (map (moveWorm g1) (zip (worms g1) randoms))}
            g3 = spawnWorms (discoveredTiles gamestate) g2
            g4 = g3 {
                  oldPlayerPos = playerPos g3
                  , discoveredTiles = Set.union (discoveredTiles g3) (Set.fromList newLos)
                  , currentWater = fillOrDecrementWater2 (currentWater g3) (desert g3) (True, playerPos g3) (maxWater (parameters g3))
                  , currentStep = currentStep g3 + 1}
            gTest = spawnWormsTM (discoveredTiles gamestate) g4
              
          in if desert g4 !! fst(playerPos g4) !! snd(playerPos g4) == "T"
              then return g4{
                          desert = replaceAt (playerPos g4) (desert g4) desertTile
                        , collectedTreasures = playerPos g4 : collectedTreasures g4}
              else return g4
    else return gamestate


randomSt :: (RandomGen g, Random a, Num a) => Control.Monad.State.State g a  
randomSt = state (randomR (0,99)) 


spawnWorms :: Set.Set Coordinate -> Gamestate ->  Gamestate
spawnWorms discoveredTiles g =
  let d = desert g  
      randomList = head (infiniteRandomLists (generators g !! currentStep g))
      maybeWormList = map (\(coord,proba) -> spawnWorm d (wormSpawn (parameters g)) coord proba) $ zip (reduceSpawnLocations (worms g) (Set.toList discoveredTiles)) randomList
      wormList = Maybe.catMaybes maybeWormList
      wormList' = map createWorm wormList
  in g{worms = worms g ++ wormList'}


------------------------------- TM

spawnWormsTM :: Set.Set Coordinate -> Gamestate -> IO Gamestate
spawnWormsTM discoveredTiles g = 
  let d = desert g  
      randomList = head (infiniteRandomLists (generators g !! currentStep g))
      maybeWormList = map (\(coord,proba) -> spawnWorm d (wormSpawn (parameters g)) coord proba) $ zip (reduceSpawnLocations (worms g) (Set.toList discoveredTiles)) randomList
      wormList = Maybe.catMaybes maybeWormList
  in do 
    tmvars <- mapM createWormTM wormList
    return g{wormsTVars = wormsTVars g ++ tmvars}

createWormTM :: Coordinate -> IO (STM.TVar Worm)
createWormTM coord = STM.newTVarIO (Worm [coord] True)

moveWormTM :: Gamestate -> (STM.TVar Worm, Int) -> IO ()
moveWormTM gamestate (worm, random) = do
  currentWorm <- STM.atomically $ STM.readTVar worm
  if length (coords currentWorm) == wormLength (parameters gamestate) || not (isEmerging currentWorm)
  then do
    let worm' = currentWorm {coords = take (length (coords currentWorm)-1) (coords currentWorm), isEmerging = False}
    return ()
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
        targetTile = selectWormDirectionTM proba finalCandidates random
    if Maybe.isJust targetTile
      then STM.atomically ( STM.writeTVar worm  (currentWorm { coords = Maybe.fromJust targetTile : coords currentWorm}))
      else STM.atomically ( STM.writeTVar worm  (currentWorm {coords = take (length (coords currentWorm)-1) (coords currentWorm), isEmerging = False}))
            
            
isValidTile :: (Coordinate, Bool, Bool) -> Bool
isValidTile (c, a, b) = a && b 

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

selectWormDirectionTM :: Int -> [(Coordinate, Bool, Bool)] -> Int -> Maybe Coordinate
selectWormDirectionTM proba candidates randomValue
  | proba == 0 = Nothing
  | proba == 100 = Just $ fst3  $ head candidates
  | proba < 100 = Just $ fst3  (candidates !! (randomValue `div` proba))
  | otherwise = Nothing
-----------------------------------TM

isDead :: Worm -> Bool
isDead w = null (coords w)

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

moveWorm :: Gamestate -> (Worm, Int) -> Worm
moveWorm gamestate (worm, random) = 
  if length (coords worm) == wormLength (parameters gamestate) || not (isEmerging worm)
  then worm {coords = take (length (coords worm)-1) (coords worm), isEmerging = False}
  else
  let d = desert gamestate
      wormHead = head (coords worm)
      adjTiles = getAdjTiles wormHead
      adjCandidates = zip adjTiles (map (\(x,y) -> d!!x!!y == "D" && not (coordElemMat (x,y) (worms gamestate))) adjTiles)
      finalCandidates = filter snd adjCandidates
      proba = ceiling ((1 / fromIntegral (length finalCandidates)) * 100)
      targetTile = selectWormDirection proba finalCandidates random
  in  if Maybe.isJust targetTile
      then worm { coords = Maybe.fromJust targetTile : coords worm}
      else worm {coords = take (length (coords worm)-1) (coords worm), isEmerging = False}
    

selectWormDirection :: Int -> [(Coordinate, Bool)] -> Int -> Maybe Coordinate
selectWormDirection proba candidates randomValue
  | proba == 0 = Nothing
  | proba == 100 = Just $ fst $ head candidates
  | proba < 100 = Just $ fst (candidates !! (randomValue `div` proba))
  | otherwise = Nothing

getAdjTiles :: Coordinate -> [Coordinate]
getAdjTiles (x,y)
    | x < 1 && y < 1  = [(x+1,y), (x,y+1)]
    | x < 1           = [(x+1,y), (x,y+1), (x,y-1)]
    | y < 1           = [(x+1,y), (x,y+1), (x-1,y)]
    | otherwise       = [(x+1,y), (x,y+1), (x,y-1), (x-1,y)]

generateRandoms :: Gamestate -> Int -> (Gamestate, [Int])
generateRandoms gamestate 0 = (gamestate, [])
generateRandoms gamestate n = 
  let (val, gen) = runState randomSt (generator gamestate)
      g = gamestate {generator = gen}
  in (g, val : snd (generateRandoms g (n-1)))


gameToString :: Gamestate -> String
gameToString g = let  res = ""
                      pos = "position " ++ makeCoordParens (playerPos g)
                      supply = "supply " ++ makeParens (show $ currentWater g)
                      revealed = unlines $ map (\x -> "revealed " ++ makeCoordParens x) (Set.toList $ discoveredTiles g)
                      collected = unlines $ map (\x -> "collected " ++ makeCoordParens x) (collectedTreasures g)
                      emerging = unlines $ map (\x -> "emerging " ++ makeParens (extractCoords x) ) (filter isEmerging (worms g))
                      disappearing = unlines $ map (\x -> "disappearing " ++ makeParens (extractCoords x) ) (filter (not . isEmerging) (worms g))
                      s = "s " ++ makeParens (show $ los $ parameters g)
                      m = "m " ++ makeParens (show $ maxWater $ parameters g)
                      g' = "g " ++ makeParens (show $ initialSeed $ parameters g)
                      t = "t " ++ makeParens (show $ treasurelh $ parameters g)
                      w = "w " ++ makeParens (show $ waterlh $ parameters g)
                      p = "p " ++ makeParens (show $ portallh $ parameters g)
                      l = "l " ++ makeParens (show $ lavalh $ parameters g)
                      ll = "ll " ++ makeParens (show $ lavalh' $ parameters g)
                      x = "x " ++ makeParens (show $ wormLength $ parameters g)
                      y = "y " ++ makeParens (show $ wormSpawn $ parameters g)
                in  (unlines[pos,supply] ++ revealed ++ collected ++ emerging ++ disappearing ++ unlines[s, m, g', t, w, p, l, ll, x, y])

makeParens :: String -> String
makeParens s =  "( " ++ s ++ " )"

makeCoord :: Coordinate -> String
makeCoord c = "[ " ++ show (fst c) ++ " , " ++  show (snd c) ++ " ]"

makeCoordParens :: Coordinate -> String
makeCoordParens c = makeParens $ makeCoord c

extractCoordsEmerging :: Worm -> String
extractCoordsEmerging worm
  | isEmerging worm = intercalate " , " (map makeCoord (coords worm))

extractCoordsDisappearing :: Worm -> String
extractCoordsDisappearing worm
  | not (isEmerging worm) = intercalate " , " (map makeCoord (coords worm))

extractCoords :: Worm -> String
extractCoords worm = intercalate " , " (map makeCoord (coords worm))


  
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