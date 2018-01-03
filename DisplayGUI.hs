module DisplayGUI
(
  makePicture
, windowWidth
, windowHeight
) where

import Desert 
import Strings
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector    as Vec

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

makePicture :: Gamestate -> Picture
makePicture gamestate = 
  let offsetX = - fromIntegral windowWidth  / 2
      offsetY = fromIntegral windowHeight / 2  - (fromIntegral tileSize +  fromIntegral tileSpace) +  fromIntegral tileSpace
      desert' = replaceAt (playerPos gamestate) (desert gamestate) "Pl"
      --subDesert = take nrLinesToDraw (map (take nrColsToDraw) desert')
      subDesert = getSubDesert desert' (fst (playerPos gamestate) - (nrLinesToDraw `div` 2)) (fst (playerPos gamestate) + (nrLinesToDraw `div` 2)) (snd (playerPos gamestate) - (nrColsToDraw `div` 2)) (snd (playerPos gamestate) + (nrColsToDraw `div` 2))
      vecDesert = Vec.fromList (concat subDesert)
      offsetLine = max 0 (fst(playerPos gamestate) - (nrLinesToDraw `div` 2))
      offsetCol = max 0 (snd(playerPos gamestate) - (nrColsToDraw `div` 2))
  in  Translate offsetX offsetY 
              $ Pictures 
              $ Vec.toList ( Vec.imap (drawTile vecDesert gamestate offsetLine offsetCol) vecDesert) 
              ++ [Translate 0 (- fromIntegral windowHeight - 5) $ Scale 0.1 0.1 $ Text ("Actual Position : (" ++ show (fst (playerPos gamestate)) ++ ", " ++ show (snd (playerPos gamestate)) ++ ")")
                 ,Translate 200 (- fromIntegral windowHeight - 5) $ Scale 0.1 0.1 $ Text ("Current Water : " ++ show (currentWater gamestate))
                 ,Translate 400 (- fromIntegral windowHeight - 5) $ Scale 0.1 0.1 $ Text ("Current Treasures : " ++ show (currentTreasures gamestate))] 


drawTile :: Vec.Vector String -> Gamestate -> Int -> Int -> Int -> String -> Picture
drawTile desert gamestate offsetLine offsetCol index tile
  = let  cs      = tileSize
         cp      = tileSpace

         (x, y)  = coordOfIndex index offsetLine offsetCol
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
              

coordOfIndex :: Int -> Int -> Int ->  (Int, Int)
coordOfIndex i offsetLine offsetCol            
        = ( i `mod` nrColsToDraw
          , i `div` nrColsToDraw )


pictureOfTile :: Gamestate -> Int -> Int -> Int -> Int -> Int -> String -> Picture
pictureOfTile gamestate x y tileSize posX posY tile
  = if (y,x) `elem` discoveredTiles gamestate
    then case tile of
         "D"   -> Color (makeColor 1.0 0.5 0.0 1.0)  (tileShape tileSize posX posY)
         "L"   -> Color (makeColor 1.0 0.0 0.0 1.0)  (tileShape tileSize posX posY)
         "W"   -> Color (makeColor 0.0 0.0 1.0 1.0)  (tileShape tileSize posX posY)
         "P"   -> Color (makeColor 0.0 0.0 0.0 1.0)  (tileShape tileSize posX posY)
         "T"   -> Color (makeColor 1.0 0.8 0.0 1.0)  (tileShape tileSize posX posY)
         "Pl"  -> Color (makeColor 1.0 1.0 1.0 1.0)  (tileShape tileSize posX posY)
         "M"   -> Color (makeColor 0.0 1.0 0.0 1.0)  (tileShape tileSize posX posY)
      else Color (makeColor 0.5 0.5 0.5 1.0)  (tileShape tileSize posX posY)


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

