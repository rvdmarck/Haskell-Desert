module EventHandlers
(
    handleEvent
) where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G
import qualified Data.Set as Set
import System.Random
import qualified Control.Concurrent.STM as STM
import Text.ParserCombinators.Parsec
import Data.List

import MapGeneration
import Desert
import Events
import Parser


type Coordinate = (Int, Int)

handleEvent :: Event -> Gamestate -> IO Gamestate
handleEvent event gamestate 
    | gameFinished (flags gamestate) = handleEventGameFinished event gamestate
    | gameStarted (flags gamestate) = handleEventGameStarted event gamestate
    | paramsLoop (flags gamestate) = handleEventParamsLoop event gamestate
    | chooseFile (flags gamestate) = handleEventChooseFile event gamestate
    | otherwise = handleEventNotGameStarted event gamestate
    

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

conditionParam :: Params -> Bool 
conditionParam p = 
    waterlh p + portallh p + lavalh p <= 100 || waterlh p + portallh p + lavalh' p <= 100


modifyParam :: Params -> Int -> Int -> Params
modifyParam params index value
    | index == 0 = let  currValue = show (los params) 
                        newValue = read (currValue ++ show value) ::Int
                        in params {los = newValue}
    | index == 1 = let  currValue = show (maxWater params) 
                        newValue = read (currValue ++ show value) ::Int
                        in params {maxWater = newValue}
    | index == 2 = let  currValue = show (initialSeed params) 
                        newValue = read (currValue ++ show value) ::Int
                        in params {initialSeed = newValue}
    | index == 3 = let  currValue = show (treasurelh params) 
                        newValue = read (currValue ++ show value) ::Int
                        in params {treasurelh = newValue}
    | index == 4 = let  currValue = show (waterlh params) 
                        newValue = read (currValue ++ show value) ::Int
                    in params {waterlh = newValue}
    | index == 5 = let  currValue = show (portallh params) 
                        newValue = read (currValue ++ show value) ::Int
                    in params {portallh = newValue}
    | index == 6 = let  currValue = show (lavalh params) 
                        newValue = read (currValue ++ show value) ::Int
                    in params {lavalh = newValue}
    | index == 7 = let  currValue = show (lavalh' params) 
                        newValue = read (currValue ++ show value) ::Int
                    in params {lavalh' = newValue}
    | index == 8 = let  currValue = show (wormLength params) 
                        newValue = read (currValue ++ show value) ::Int
                    in params {wormLength = newValue}
    | index == 9 = let  currValue = show (wormSpawn params) 
                        newValue = read (currValue ++ show value) ::Int
                        in params {wormSpawn = newValue}
    | otherwise  = params


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
    , currentParam = 10
    , flags = GameFlags True False False False False False
    }

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
            0
            ""
            (GameFlags True False False False False False))


gameToString :: Gamestate -> String
gameToString g = let    res = ""
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