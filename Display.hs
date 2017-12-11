module Display
(
  printMatrix
, printInfos
) where

import Strings
import BFS
import Desert

type PlayerPos = (Int, Int)



printMatrix :: [[(Bool, String)]] -> Int -> Int -> Int -> Int -> PlayerPos -> IO()
printMatrix desert startRow endRow startCol endCol ppos = do
  let d = replaceAt ppos desert playerString
  --let los = getLos ppos 2
  if startRow < 0
    then
      if startCol < 0
        then mapM_ printDesertLine (makeSubMatrix d (max 0 startRow) (endRow + abs startRow) (max 0 startCol) (endCol + abs startCol))
        else mapM_ printDesertLine (makeSubMatrix d (max 0 startRow) (endRow + abs startRow) (max 0 startCol) endCol)
    else
      if startCol < 0
        then mapM_ printDesertLine (makeSubMatrix d (max 0 startRow) endRow (max 0 startCol) (endCol + abs startCol))
        else mapM_ printDesertLine (makeSubMatrix d (max 0 startRow) endRow (max 0 startCol) endCol)
  return()


makeSubMatrix :: [[(Bool, String)]] -> Int -> Int -> Int -> Int -> [[(Bool, String)]]
makeSubMatrix desert startRow endRow startCol endCol
  | startRow == endRow = []
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix desert (startRow + 1) endRow startCol endCol

printDesertLine :: [(Bool, String)] -> IO()
printDesertLine l =
  let l' = makePrintableDesertLine l
  in putStrLn (unwords l')

makePrintableDesertLine :: [(Bool, String)] -> [String]
makePrintableDesertLine = map (\(draw, val) ->
  if draw
    then
      if val == treasureTile
        then desertTile
        else val
    else " ")


printInfos :: [[(Bool, String)]] -> (Int, Int) -> Int -> Int -> IO ()
printInfos desert' ppos currentWater currentTreasures = do
  printMatrix desert' (fst ppos - 5) (fst ppos + 6) (snd ppos - 5) (snd ppos + 6) ppos
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
printDist :: [[(Bool, String)]] -> PlayerPos -> String -> String -> IO ()
printDist desert' ppos value message = do
  let distToValue = bfs desert' (ppos, 0) value [] [(ppos, 0)]
  putStr message
  print distToValue

drawPlayerPos :: PlayerPos -> IO()
drawPlayerPos pos = putStr ("(" ++ show (fst pos) ++ "," ++ show(snd pos) ++ ")")
