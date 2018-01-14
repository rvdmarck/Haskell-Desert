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
import Display
import DisplayGUI
import Desert
import Parser
import Worm
import Events


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
              Set.empty                                          -- coordinates of discovered tiles
              []                                          -- coordinates of collected treasures
              []                                          -- worms list
              []                                          -- infinite generators (used to spawn worms)
              0                                           -- current step
              (mkStdGen 7)                                -- single generator 
              []
              (-30,250)
              0
              ""
              (GameFlags False False False False False False) )
          makePicture 
          handleEvent 
          stepWorld


launchGameFromFile :: ParseInfos -> IO Gamestate
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
  wormListTVar <- mapM STM.newTVarIO  wormList
  return (Gamestate 
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
            (mkStdGen 7)                                -- single generator
            wormListTVar
            (0,0)
            0
            ""
            (GameFlags True False False False False False))

handleEvent :: Event -> Gamestate -> IO Gamestate
handleEvent event gamestate 
  | gameFinished (flags gamestate) = handleEventGameFinished event gamestate
  | gameStarted (flags gamestate) = handleEventGameStarted event gamestate
  | paramsLoop (flags gamestate) = handleEventParamsLoop event gamestate
  | chooseFile (flags gamestate) = handleEventChooseFile event gamestate
  | otherwise = handleEventNotGameStarted event gamestate

handleEventParamsLoop :: Event -> Gamestate -> IO Gamestate
handleEventParamsLoop event gamestate = 
  let val = getNumEvent event
  in if val /= "enter"
        then if val /= "undefined"
              then let param = modifyParam (parameters gamestate) (currentParam gamestate) (read val)
                   in return gamestate {parameters = param}
              else return gamestate
        else if currentParam gamestate < 9
          then return gamestate {currentParam = currentParam gamestate + 1}
          else if conditionParam (parameters gamestate)
                then initGamestate gamestate
                else return gamestate {parameters = Params 0 0 0 0 0 0 0 0 0 0, currentParam = 0}

modifyParam :: Params -> Int -> Int -> Params
modifyParam params index value
    | index == 0 = let currValue = show (los params) 
                       newValue = read (currValue ++ show value) ::Int
                       in params {los = newValue}
    | index == 1 = let currValue = show (maxWater params) 
                       newValue = read (currValue ++ show value) ::Int
                      in params {maxWater = newValue}
    | index == 2 = let currValue = show (initialSeed params) 
                       newValue = read (currValue ++ show value) ::Int
                      in params {initialSeed = newValue}
    | index == 3 = let currValue = show (treasurelh params) 
                       newValue = read (currValue ++ show value) ::Int
                      in params {treasurelh = newValue}
    | index == 4 = let currValue = show (waterlh params) 
                       newValue = read (currValue ++ show value) ::Int
                    in params {waterlh = newValue}
    | index == 5 = let currValue = show (portallh params) 
                       newValue = read (currValue ++ show value) ::Int
                    in params {portallh = newValue}
    | index == 6 = let currValue = show (lavalh params) 
                       newValue = read (currValue ++ show value) ::Int
                    in params {lavalh = newValue}
    | index == 7 = let currValue = show (lavalh' params) 
                       newValue = read (currValue ++ show value) ::Int
                    in params {lavalh' = newValue}
    | index == 8 = let currValue = show (wormLength params) 
                       newValue = read (currValue ++ show value) ::Int
                    in params {wormLength = newValue}
    | index == 9 = let currValue = show (wormSpawn params) 
                       newValue = read (currValue ++ show value) ::Int
                      in params {wormSpawn = newValue}
    | otherwise  = params

conditionParam :: Params -> Bool 
conditionParam p = 
  waterlh p + portallh p + lavalh p <= 100 || waterlh p + portallh p + lavalh' p <= 100

handleEventGameStarted :: Event -> Gamestate -> IO Gamestate
handleEventGameStarted event gamestate 
  | EventKey (G.Char 'd') Down _ _ <- event
  = return gamestate {playerPos = (fst(playerPos gamestate), snd(playerPos gamestate) + 1)}

  | EventKey (G.Char 's') Down _ _ <- event
  = return gamestate {playerPos = (fst(playerPos gamestate) + 1, snd(playerPos gamestate))}

  | EventKey (G.Char 'w') Down _ _ <- event
  = if fst(playerPos gamestate) > 0
      then return gamestate {playerPos = (fst(playerPos gamestate) - 1, snd(playerPos gamestate))}
      else return gamestate

  | EventKey (G.Char 'a') Down _ _ <- event
  = if snd(playerPos gamestate) > 0
      then return gamestate {playerPos = (fst(playerPos gamestate), snd(playerPos gamestate) - 1)}
      else return gamestate

  | EventKey (SpecialKey KeyF5) Down _ _ <- event
  = let s = gameToString gamestate
    in return gamestate {flags = (flags gamestate) {gameStarted = False, chooseFile = True, saving = True}}

  | EventKey (SpecialKey KeyF9) Down _ _ <- event
  = let s = gameToString gamestate
    in return gamestate {flags = (flags gamestate) {gameStarted = False, chooseFile = True, saving = False}}

  | otherwise
  = return gamestate

handleEventNotGameStarted :: Event -> Gamestate -> IO Gamestate
handleEventNotGameStarted event gamestate 
  | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event =
      if x >= -50 && x <= 50 && y >= 75 && y <= 125
        then return gamestate {flags = (flags gamestate) {paramsLoop = True}}
        else 
          if x >= -50 && x <= 50 && y >= -25 && y <= 25
            then return gamestate {flags = (flags gamestate) {chooseFile = True}}
            else
              return gamestate
  | otherwise = return gamestate


handleEventChooseFile :: Event -> Gamestate -> IO Gamestate
handleEventChooseFile event gamestate = 
  let val = getAlphaEvent event
  in if val /= "enter"
        then if val /= "undefined"
              then return gamestate {currentFileName = currentFileName gamestate ++ val}
              else return gamestate
        else if saving (flags gamestate)
              then do
                let s = gameToString gamestate
                writeFile (currentFileName gamestate) s
                return gamestate {flags = (flags gamestate) {chooseFile = False, gameStarted = True}, currentFileName = ""}
              else do parseInfos <-  parseFromFile gameParser (currentFileName gamestate)
                      case parseInfos of
                        Left err -> return gamestate
                        Right parseInfos -> launchGameFromFile parseInfos

handleEventGameFinished :: Event -> Gamestate -> IO Gamestate
handleEventGameFinished event gamestate = return gamestate
                    
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
      g4 <- spawnWorms (discoveredTiles gamestate) g3
      let endGame = checkEndGame2
      if checkEndGame2 gamestate == 1
        then return gamestate{flags = (flags gamestate) {playerDead = True, gameFinished = True}}
        else if checkEndGame2 gamestate == 2
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


initGamestate :: Gamestate -> IO Gamestate
initGamestate gamestate = do
  let params = parameters gamestate
      genList = infiniteGenerators (mkStdGen 33)
      tileList = initTileList params
      randomTiles = randomDesert (mkStdGen (initialSeed params)) tileList (repeat "A") params genList
      randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
      randomTreasuresTiles = (map . map) (corresp params) randomTreasures
      desert' = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles 
  return gamestate{
      desert = desert'
    , parameters = params
    , currentWater = maxWater (parameters gamestate)
    , discoveredTiles = Set.fromList (getLos (0,0) (los(parameters gamestate)))
    , generators = infiniteGenerators (mkStdGen 42)
    , cursorCoordinate = (0,0)
    , currentParam = 10
    , flags = GameFlags True False False False False False
  }

































  
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

checkEndGame2 :: Gamestate -> Int
checkEndGame2 gamestate 
  | desert gamestate !! fst (playerPos gamestate) !! snd (playerPos gamestate) `elem` [lavaTile, "L'"] 
    || currentWater gamestate == 0 
    || coordElemMat (playerPos gamestate) (worms gamestate) = 1
  | desert gamestate !! fst (playerPos gamestate) !! snd (playerPos gamestate) == portalTile = 2
  | otherwise = 0