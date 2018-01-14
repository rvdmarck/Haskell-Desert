module DisplayGUI
(
  makePicture
, windowWidth
, windowHeight
, coordElemMat
, coordElemMatTM
) where

import Desert 
import Strings
import BFS
import MapGeneration
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as Vec
import qualified Control.Concurrent.STM as STM

type Coordinate = (Int, Int)
type Desert = [[String]]

nrLinesToDraw = 50
nrColsToDraw = 50
tileSize = 10
tileSpace = 1
windowWidth :: Int
windowWidth = fst computeWindowSize
windowHeight :: Int
windowHeight = snd computeWindowSize

makePicture :: Gamestate -> IO Picture
makePicture gamestate 
  | gameFinished (flags gamestate) = return (makePictureGameFinished gamestate)
  | paramsLoop (flags gamestate) = return (makePictureParamsLoop gamestate)
  | chooseFile (flags gamestate) = return (makePictureChooseFile gamestate)
  | gameStarted (flags gamestate) = 
      let offsetX = - fromIntegral windowWidth  / 2
          offsetY = fromIntegral windowHeight / 2  - fromIntegral tileSize
          desert' = replaceAt (playerPos gamestate) (desert gamestate) "Pl"
          subDesert = getSubDesert desert' (fst (playerPos gamestate) - (nrLinesToDraw `div` 2)) (fst (playerPos gamestate) + (nrLinesToDraw `div` 2)) (snd (playerPos gamestate) - (nrColsToDraw `div` 2)) (snd (playerPos gamestate) + (nrColsToDraw `div` 2))
          vecDesert = Vec.fromList (concat subDesert)
          offsetLine = max 0 (fst(playerPos gamestate) - (nrLinesToDraw `div` 2))
          offsetCol = max 0 (snd(playerPos gamestate) - (nrColsToDraw `div` 2))
      in return (Translate offsetX offsetY 
                  $ Pictures 
                  $ Vec.toList ( Vec.imap (drawTile vecDesert gamestate offsetLine offsetCol) vecDesert) 
                  ++ [Translate 0 (- fromIntegral windowHeight - 5) $ Scale 0.1 0.1 $ Text ("Actual Position : (" ++ show (fst (playerPos gamestate)) ++ ", " ++ show (snd (playerPos gamestate)) ++ ")")
                    , Translate 200 (- fromIntegral windowHeight - 5) $ Scale 0.1 0.1 $ Text ("Current Water : " ++ show (currentWater gamestate))
                    , Translate 400 (- fromIntegral windowHeight - 5) $ Scale 0.1 0.1 $ Text ("Current Treasures : " ++ show (length $ collectedTreasures gamestate))
                    , Translate 0 (- fromIntegral windowHeight - 25) $ Scale 0.1 0.1 $ Text ("Nearest Water : " ++ show (bfs (desert gamestate) (playerPos gamestate, 0) waterTile [] [(playerPos gamestate, 0)]))
                    , Translate 200 (- fromIntegral windowHeight - 25) $ Scale 0.1 0.1 $ Text ("Nearest Treasure : " ++ show (bfs (desert gamestate) (playerPos gamestate, 0) treasureTile [] [(playerPos gamestate, 0)]))
                    , Translate 400 (- fromIntegral windowHeight - 25) $ Scale 0.1 0.1 $ Text ("Nearest Portal : " ++ show (bfs (desert gamestate) (playerPos gamestate, 0) portalTile [] [(playerPos gamestate, 0)]))] 
                )
  | otherwise = return (makePictureNotGameStarted gamestate)


makePictureParamsLoop :: Gamestate -> Picture
makePictureParamsLoop gamestate =
  let range = take 10 [250, 200..]
      params = ["S : " ++ show (los (parameters gamestate))
              , "M : " ++ show (maxWater (parameters gamestate))
              , "G : " ++ show (initialSeed (parameters gamestate))
              , "T : " ++ show (treasurelh (parameters gamestate))
              , "W : " ++ show (waterlh (parameters gamestate))
              , "P : " ++ show (portallh (parameters gamestate))
              , "L : " ++ show (lavalh (parameters gamestate))
              , "LL : " ++ show (lavalh' (parameters gamestate))
              , "X : " ++ show (wormLength (parameters gamestate))
              , "Y : " ++ show (wormSpawn (parameters gamestate))]
      rangeParams = zip range params
      pictures = map (\(y, text) -> writeAt (-40) y text) rangeParams
  in 
  Pictures pictures

writeAt :: Float -> Float -> String -> Picture
writeAt x y text  = 
  Translate x y $ Scale 0.1 0.1 $ Text text


makePictureChooseFile :: Gamestate -> Picture
makePictureChooseFile gamestate =
  Pictures [Translate (-40) 95 $ Scale 0.1 0.1 $ Text "Enter filename : "
          , Translate (-40) (-5) $ Scale 0.1 0.1 $ Text (currentFileName gamestate)]

makePictureNotGameStarted :: Gamestate -> Picture
makePictureNotGameStarted gamestate =
  Pictures [Translate 0 100 $ rectangleWire 100 50
          , Translate (-40) 95 $ Scale 0.1 0.1 $ Text "START GAME"
          , Translate 0 0 $ rectangleWire 100 50
          , Translate (-40) (-5) $ Scale 0.1 0.1 $ Text "LOAD GAME"]
    
makePictureGameFinished :: Gamestate -> Picture
makePictureGameFinished gamestate = 
  Pictures [Translate (-40) 95 $ Scale 0.1 0.1 $ Text "GAME FINISHED "
          , Translate (-40) (-5) $ Scale 0.1 0.1 $ Text finalMessage]
  where finalMessage = if playerDead (flags gamestate)
                        then "You are dead !"
                        else "You won !"

drawTile :: Vec.Vector String -> Gamestate -> Int -> Int -> Int -> String -> Picture
drawTile desert gamestate offsetLine offsetCol index tile
  = let  cs      = tileSize
         cp      = tileSpace

         (x, y)  = coordOfIndex index
         fx      = fromIntegral x * (cs + cp) + 1
         fy      = fromIntegral y * (cs + cp) + 1

    in   pictureOfTile
                gamestate
                (x + offsetCol)
                (y + offsetLine)
                tileSize
                fx
                (-fy)
                tile
              

coordOfIndex :: Int ->  (Int, Int)
coordOfIndex i           
        = ( i `mod` nrColsToDraw
          , i `div` nrColsToDraw )


pictureOfTile :: Gamestate -> Int -> Int -> Int -> Int -> Int -> String -> Picture
pictureOfTile gamestate x y tileSize posX posY tile
  | coordElemMat (y,x) (worms gamestate) && (y,x) `elem` discoveredTiles gamestate =
    Color (makeColor 0.0 1.0 0.0 1.0)  (tileShape tileSize posX posY)
  | (y,x) `elem` discoveredTiles gamestate =
    case tile of
      "D"   -> Color (makeColor 1.0 0.5 0.0 1.0)  (tileShape tileSize posX posY)
      "L"   -> Color (makeColor 1.0 0.0 0.0 1.0)  (tileShape tileSize posX posY)
      "W"   -> Color (makeColor 0.0 0.0 1.0 1.0)  (tileShape tileSize posX posY)
      "P"   -> Color (makeColor 0.0 0.0 0.0 1.0)  (tileShape tileSize posX posY)
      "T"   -> Color (makeColor 1.0 0.5 0.0 1.0)  (tileShape tileSize posX posY)
      "Pl"  -> Color (makeColor 1.0 1.0 1.0 1.0)  (tileShape tileSize posX posY)
      "M"   -> Color (makeColor 0.0 1.0 0.0 1.0)  (tileShape tileSize posX posY)
   | not ((y,x) `elem` discoveredTiles gamestate) = 
      Color (makeColor 0.5 0.5 0.5 1.0)  (tileShape tileSize posX posY)


coordElemMat :: Coordinate -> [Worm] -> Bool
coordElemMat coord worms = 
  let wormsCoords = map coords worms
      bools = map (elem coord) wormsCoords
  in True `elem` bools


coordElemMatTM :: Coordinate -> [STM.TVar Worm] -> IO Bool
coordElemMatTM coord worms = do 
  worms' <- mapM (STM.atomically . STM.readTVar) worms
  let   wormsCoords = map coords worms'
        bools = map (elem coord) wormsCoords
    in return (True `elem` bools)


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



makeSubMatrix :: Desert -> Int -> Int -> Int -> Int -> Desert
makeSubMatrix desert startRow endRow startCol endCol
  | startRow == endRow = []
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix desert (startRow + 1) endRow startCol endCol


getSubDesert :: Desert -> Int -> Int -> Int -> Int -> Desert
getSubDesert desert startRow endRow startCol endCol
  | startRow < 0 =
    if startCol < 0 
      then
        makeSubMatrix desert (max 0 startRow) (endRow + abs startRow) (max 0 startCol) (endCol + abs startCol)
      else
        makeSubMatrix desert (max 0 startRow) (endRow + abs startRow) (max 0 startCol) endCol
  | startCol < 0 =
    makeSubMatrix desert (max 0 startRow) endRow (max 0 startCol) (endCol + abs startCol)
  | otherwise =
    makeSubMatrix desert (max 0 startRow) endRow (max 0 startCol) endCol

