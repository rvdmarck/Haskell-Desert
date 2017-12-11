module BFS
(
  bfs
, bfsStrict
) where

import Strings

type Desert = [[String]]
type Coordinate = (Int, Int)

-- Module performing Breadth First Search Algorithm

bfs :: Desert -> (Coordinate, Int) -> String -> [(Coordinate, Int)] -> [(Coordinate, Int)] -> Int
bfs desert (ppos, dist) value queue marked =
  let adj = getAdj desert (ppos, dist) marked
   in let result = checkTermination desert adj value
   in subbfs desert result value queue marked adj

subbfs :: Desert -> (Bool, Int) -> String -> [(Coordinate, Int)] -> [(Coordinate, Int)] -> [(Coordinate, Int)] -> Int
subbfs desert result value queue marked adj
    | fst result = snd result
    | null queue =
      let queue' = [] ++ adj in
        if null queue' then (-1) else
          bfs desert (head queue') value queue' (marked ++ [head queue'])
    | otherwise =
      let queue' = tail queue ++ adj in
        if null queue' then (-1) else
          bfs desert (head queue') value queue' (marked ++ [head queue'])

-- Get the top, right, bottom and left positions of a given position wrt there is no negative coordinate and the positions doesn't contain lava
getAdj :: Desert -> (Coordinate, Int) -> [(Coordinate, Int)] -> [(Coordinate, Int)]
getAdj desert (pos, dist) marked =
  [((row, col), dist + 1) |
  row <- [fst pos - 1 .. fst pos + 1],
  col <- [snd pos - 1 .. snd pos + 1],
  abs((row + col) - uncurry (+) pos) == 1,
  row >= 0 && col >= 0,
  desert!!row!!col /= lavaTile,
  (row,col) `notElem` map fst marked]

-- If an adjacent tile contain the target value, we stop
checkTermination :: Desert -> [(Coordinate, Int)] -> String -> (Bool, Int)
checkTermination desert adj value =
  let x = map (\((row, col), dist) -> (desert!!row!!col == value, dist)) adj
  in if True `elem` map fst x
    then
      let y = filter fst x
      in head y
      else (False, 0)


-- Breadth First Search using `seq` operator
bfsStrict :: Desert -> (Coordinate, Int) -> String -> [(Coordinate, Int)] -> [(Coordinate, Int)] -> Int
bfsStrict desert (ppos, dist) value queue marked =
  let adj = getAdj desert (ppos, dist) marked
   in let result = checkTermination desert adj value
   in desert `seq` subbfsStrict desert result value queue marked adj

subbfsStrict :: Desert -> (Bool, Int) -> String -> [(Coordinate, Int)] -> [(Coordinate, Int)] -> [(Coordinate, Int)] -> Int
subbfsStrict desert result value queue marked adj
    | fst result = snd result
    | null queue =
      let queue' = [] ++ adj in
        if null queue' then (-1) else
          desert `seq` bfsStrict desert (head queue') value queue' (marked ++ [head queue'])
    | otherwise =
      let queue' = tail queue ++ adj in
        if null queue' then (-1) else
          desert `seq` bfsStrict desert (head queue') value queue' (marked ++ [head queue'])
