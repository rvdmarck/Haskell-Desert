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
import Desert

type Vec        = Vec.Vector

type Desert = [[String]]
type Coordinate = (Int, Int)

windowWidth :: Int
windowWidth = fst computeWindowSize
windowHeight :: Int
windowHeight = snd computeWindowSize


main :: IO ()
main = do
  let ppos = (0,0)
  let params = Params 3 50 12 10 25 15 15 20
  --params <- paramsLoop
  let tileList = initTileList params
  let genList = infiniteGenerators (mkStdGen 33)
  let randomTiles = randomDesert (mkStdGen (initialSeed params)) tileList (repeat "A") params genList
  let randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
  let randomTreasuresTiles = (map . map) (corresp params) randomTreasures
  let desert = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles
  play    (InWindow "Desert Game" (windowWidth,windowHeight) (500,500)) white 100 desert makePicture handleEvent stepWorld

  --gameLoop ppos desert params (maxWater params) 0 []


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











nrLinesToDraw = 20
nrColsToDraw = 20
tileSize = 10
tileSpace = 1

makePicture :: Desert -> Picture
makePicture desert = 
  let offsetX = - fromIntegral windowWidth  / 2
      offsetY = fromIntegral windowHeight / 2  - (fromIntegral tileSize +  fromIntegral tileSpace) +  fromIntegral tileSpace
      subDesert = take nrLinesToDraw (map (take nrColsToDraw) desert)
      cSubDesert = concat subDesert
      vecDesert = Vec.fromList cSubDesert
  in  Translate offsetX offsetY
        $ Pictures 
        $ Vec.toList 
        $ Vec.imap (drawTile vecDesert) vecDesert    


drawTile :: Vec.Vector String -> Int -> String -> Picture
drawTile desert index tile
  = let  cs      = tileSize
         cp      = tileSpace

         (x, y)  = coordOfIndex index
         fx      = fromIntegral x * (cs + cp) + 1
         fy      = fromIntegral y * (cs + cp) + 1

    in   pictureOfTile
                tileSize
                fx
                (-fy)
                tile
              

coordOfIndex :: Int -> (Int, Int)
coordOfIndex i            
        = ( i `mod` nrColsToDraw
          , i `div` nrColsToDraw)


pictureOfTile :: Int -> Int -> Int -> String -> Picture
pictureOfTile tileSize posX posY tile
  = case tile of
         "D"    -> Color (makeColor 1.0 0.5 0.0 1.0)  (tileShape tileSize posX posY)
         "L"      -> Color (makeColor 1.0 0.0 0.0 1.0)  (tileShape tileSize posX posY)
         "W"     -> Color (makeColor 0.0 0.0 1.0 1.0)  (tileShape tileSize posX posY)
         "P"    -> Color (makeColor 0.0 0.0 0.0 1.0)  (tileShape tileSize posX posY)
         "T"  -> Color (makeColor 1.0 0.8 0.0 1.0)  (tileShape tileSize posX posY)


-- | The basic shape of a tile.
tileShape :: Int -> Int -> Int -> Picture
tileShape tileSize posXi posYi
 = let  cs      = fromIntegral tileSize
        posX    = fromIntegral posXi
        posY    = fromIntegral posYi
        x1      = posX
        x2      = posX + cs
        y1      = posY 
        y2      = posY + cs
   in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]


-- | Get the size of the window needed to display a world.
computeWindowSize :: (Int, Int)
computeWindowSize
 = let  tilePad         = tileSize + tileSpace
        height          = tilePad * nrLinesToDraw + tileSpace
        width           = tilePad * nrColsToDraw + tileSpace
   in   (width, height)


handleEvent :: Event -> Desert -> Desert
handleEvent _ = id


stepWorld :: Float -> Desert -> Desert
stepWorld _ = id