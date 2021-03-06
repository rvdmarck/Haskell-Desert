module Display
(
  printMatrix
, printInfos
) where

import Strings
import BFS
import Desert

type Coordinate = (Int, Int)
type Desert = [[String]]

printMatrix :: Desert -> Int -> Int -> Int -> Int -> Coordinate -> [Coordinate] -> IO()
printMatrix desert startRow endRow startCol endCol ppos uncoveredTilesCoord = do
  let d = replaceAt ppos desert playerString
  if startRow < 0
    then
      if startCol < 0
        then do
          let sub = makeSubMatrix d (max 0 startRow) (endRow + abs startRow) (max 0 startCol) (endCol + abs startCol)
          printSubMatrix uncoveredTilesCoord (max 0 startRow) (endRow + abs startRow) (max 0 startCol) (endCol + abs startCol) sub 0
        else do
          let sub = makeSubMatrix d (max 0 startRow) (endRow + abs startRow) (max 0 startCol) endCol
          printSubMatrix uncoveredTilesCoord (max 0 startRow) (endRow + abs startRow) (max 0 startCol) endCol sub 0
    else
      if startCol < 0
        then do
          let sub = makeSubMatrix d (max 0 startRow) endRow (max 0 startCol) (endCol + abs startCol)
          printSubMatrix uncoveredTilesCoord (max 0 startRow) endRow (max 0 startCol) (endCol + abs startCol) sub 0
        else do
          let sub = makeSubMatrix d (max 0 startRow) endRow (max 0 startCol) endCol
          printSubMatrix uncoveredTilesCoord (max 0 startRow) endRow (max 0 startCol) endCol sub 0
  return()

printSubMatrix :: [Coordinate] -> Int -> Int -> Int -> Int -> [[String]] -> Int -> IO()
printSubMatrix uncoveredTilesCoord startRow endRow startCol endCol subDesert cnt
  | startRow == endRow = return()
  | otherwise = do
    printDesertLine uncoveredTilesCoord startRow startCol endCol (subDesert !! cnt)
    printSubMatrix uncoveredTilesCoord (startRow + 1) endRow startCol endCol subDesert (cnt+1)


makeSubMatrix :: Desert -> Int -> Int -> Int -> Int -> Desert
makeSubMatrix desert startRow endRow startCol endCol
  | startRow == endRow = []
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix desert (startRow + 1) endRow startCol endCol

printDesertLine :: [Coordinate] -> Int -> Int -> Int -> [String] -> IO()
printDesertLine uncoveredTilesCoord row startCol endCol l = 
  let l' = makePrintableDesertLine l uncoveredTilesCoord (row, startCol) startCol endCol
  in putStrLn (unwords l')

makePrintableDesertLine :: [String] -> [Coordinate] -> Coordinate -> Int -> Int -> [String] 
makePrintableDesertLine desertLine uncoveredTilesCoord (row, startCol) startCol' endCol
  | startCol == endCol = []
  | otherwise =  makePrintableElement desertLine uncoveredTilesCoord (row, startCol) startCol' : makePrintableDesertLine desertLine uncoveredTilesCoord (row, startCol + 1) startCol' endCol

makePrintableElement :: [String] -> [Coordinate] -> Coordinate -> Int -> String
makePrintableElement desertLine uncoveredTilesCoord currCoord startCol
  | currCoord `elem` uncoveredTilesCoord = if desertLine !! (snd currCoord - startCol) == treasureTile then desertTile else desertLine !! (snd currCoord - startCol)
  | otherwise = " "


printInfos :: Desert -> Coordinate -> Int -> Int -> [Coordinate] -> IO ()
printInfos desert' ppos currentWater currentTreasures uncoveredTilesCoord = do
  printMatrix desert' (fst ppos - 5) (fst ppos + 6) (snd ppos - 5) (snd ppos + 6) ppos uncoveredTilesCoord
  putStrLn separator

  printDist desert' ppos waterTile distClosestToW
  printDist desert' ppos treasureTile distClosestToT
  printDist desert' ppos portalTile distClosestToP

  putStr "Actual Position : "
  drawPlayerPos ppos
  putStr " , Current Water : "
  putStr (show currentWater)
  putStr " , Current Treasures : "
  print currentTreasures
  putStrLn separator
  putStrLn "w,a,s,d : "

-- Given a position and a target tile value, gives the number of steps to reach it, respect to lavas
printDist :: Desert -> Coordinate -> String -> String -> IO ()
printDist desert' ppos value message = do
  let distToValue = bfs desert' (ppos, 0) value [] [(ppos, 0)]
  putStr message
  print distToValue

drawPlayerPos :: Coordinate -> IO()
drawPlayerPos pos = putStr ("(" ++ show (fst pos) ++ "," ++ show(snd pos) ++ ")")
