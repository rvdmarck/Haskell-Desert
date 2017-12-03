import System.Environment()
import System.Random
import Control.Monad.State
import Data.List
import Text.Read

{--
data Tile = DESERT | LAVA | TREASURE | WATER
            deriving (Show, Read)
--}

data Params = Params { los :: Int
                   , maxWater :: Int
                   , initialSeed :: Int
                   , treasurelh :: Int
                   , waterlh :: Int
                   , portallh :: Int
                   , lavalh :: Int
                   , lavalh' :: Int
                   } deriving (Show)

type Desert = [[Int]]
type Sdesert = [[String]]
type PlayerPos = (Int, Int)

infiniteGenerators :: (RandomGen g) => g -> [g]
infiniteGenerators = unfoldr (Just . split)

infiniteRandomLists :: (RandomGen g) => g -> [[Int]]
infiniteRandomLists = map (randomRs (0,99)) . infiniteGenerators

main :: IO ()
main = do
  let ppos = (0,0)
  --params <- paramsLoop
  let params = Params 5 80 1 10 20 0 0 0
  let tileList = initTileList params

  --let randomTiles = map (randomTileLine tileList "A" params . mkStdGen) [(initialSeed params)..]

  let randomTiles = randomDesert (mkStdGen (initialSeed params)) tileList (repeat "A") params

  let randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
  let randomTreasuresTiles = (map . map) (corresp' params) randomTreasures
  let desert = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles
  let desert' = (map . map) initDiscovered desert
  gameLoop ppos desert' params (maxWater params) 0

initDiscovered :: String -> (Bool, String)
initDiscovered tile = (False, tile)

randomDesert :: StdGen -> [String] -> [String] -> Params -> [[String]]
randomDesert gen tileList precTileLine params =
  let currentTileLine = randomTileLine tileList "A" params precTileLine gen
    in let (_, newGen) = random gen::(Int, StdGen)
      in currentTileLine : randomDesert newGen tileList currentTileLine params


randomTileLine :: [String] -> String -> Params -> [String] -> StdGen -> [String]
randomTileLine tileList precTile params (x:xs) gen =
   let (tile, newGen) = randomTile gen tileList precTile params x
    in tile : randomTileLine tileList tile params xs newGen

randomTile :: StdGen -> [String] -> String -> Params -> String -> (String, StdGen)
randomTile g tileList precTile params aboveTile =
  let (proba, gen) = randomR (0,99) g::(Int, StdGen)
    in (correspTile proba tileList precTile params gen aboveTile, gen)

correspTile :: Int -> [String] -> String -> Params -> StdGen -> String -> String
correspTile proba tileList precTile params gen aboveTile
  | tileList !! proba /= "D" = tileList !! proba
  | otherwise =
    let (newProba, _) = randomR (0,99) gen
    in
    if precTile == "L" || aboveTile == "L"
      then
        if newProba < lavalh' params
          then "L"
          else "D"
      else
        if newProba < lavalh params
          then "L"
          else "D"


corresp :: [String] -> Int -> String
corresp tileList proba
  | tileList !! proba /= "D" = tileList !! proba
  | otherwise = "D"

corresp' :: Params -> Int -> String
corresp' params proba
  | proba < treasurelh params = "T"
  | otherwise = "D"

compareTreasure :: String -> String -> String
compareTreasure x y  =
  if x == "D"
    then
      if y =="T"
        then "T"
        else "D"
    else x

initTileList :: Params -> [String]
initTileList params =
  let tmpList = replicate (waterlh params) "W" ++ replicate (portallh params) "P"
    in tmpList ++ replicate (100 - length tmpList) "D"

printMatrix :: [[(Bool, String)]] -> Int -> Int -> Int -> Int -> PlayerPos -> IO()
printMatrix desert startRow endRow startCol endCol ppos = do
  let d = replaceAt ppos desert "\2000"
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
printDesertLine l = --print (unwords l)
  let l' = makePrintableDesertLine l
  in putStrLn (unwords l')

makePrintableDesertLine :: [(Bool, String)] -> [String]
makePrintableDesertLine = map (\(draw, val) ->
  if draw
    then
      if val == "T"
        then "D"
        else val
    else " ")


replaceAt :: PlayerPos -> [[(Bool, String)]] -> String -> [[(Bool, String)]]
replaceAt ppos desert val =
  let (x,_:ys) = splitAt (snd ppos) (desert !! fst ppos)
    in let (x',_ : ys') = splitAt (fst ppos) desert
      in x' ++ [x ++ (True, val) : ys] ++ ys'


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




gameLoop :: PlayerPos -> [[(Bool, String)]] -> Params -> Int -> Int -> IO ()
gameLoop ppos desert params currentWater currentTreasures = do
  let los' = getLos ppos (los params)
  desert' <- uncoverTiles desert los'
  printMatrix desert' (fst ppos - 5) (fst ppos + 6) (snd ppos - 5) (snd ppos + 6) ppos

  let distWater = bfs desert' (ppos, 0) "W" [] [(ppos, 0)]
  putStr "Distance is : "
  putStrLn (show distWater)

  putStr "Actual Position : "
  drawPlayerPos ppos
  putStr " , Current Water : "
  putStr (show currentWater)
  putStr " , Current Treasures : "
  putStrLn (show currentTreasures)

  putStrLn "w,a,s,d : "
  input <- getLine
  newpos <- doMove input ppos
  --drawPlayerPos newpos
  currentWater' <- fillOrDecrementWater currentWater desert' newpos (maxWater params)
  endGame <- checkEndGame desert' (snd newpos) currentWater'
  currentTreasures' <- checkTreasureFound currentTreasures (snd(desert' !! fst(snd newpos) !! snd (snd newpos)))


  if currentTreasures' /= currentTreasures
    then do
      desert'' <- pickUpTreasure desert' (snd newpos)
      _ <- return desert''
      if endGame == 0
        then gameLoop (snd newpos) desert'' params currentWater' currentTreasures'
        else
          if endGame == 1
            then putStrLn "You're DEAD !"
            else
              when  (endGame == 2) $
                putStrLn "You WON !"


    else do
      desert'' <- returnDesert desert'
      _ <- return desert''
      if endGame == 0
        then gameLoop (snd newpos) desert'' params currentWater' currentTreasures'
        else
          if endGame == 1
            then putStrLn "You're DEAD !"
            else
              when  (endGame == 2) $
                putStrLn "You WON !"


getLos :: PlayerPos -> Int -> [PlayerPos]
getLos ppos llos =
  let xs = [-llos..llos]
    in [(x + fst ppos, y + snd ppos) | x <- xs, y <- xs, abs x + abs y <= llos, x + fst ppos >= 0 && y + snd ppos >= 0]


uncoverTiles :: [[(Bool, String)]] -> [(Int, Int)] -> IO [[(Bool, String)]]
uncoverTiles desert [] = return desert
uncoverTiles desert losPos =
  let desert' = replaceAt (head losPos) desert (snd(desert!!fst (head losPos)!!snd (head losPos)))
  in uncoverTiles desert' (drop 1 losPos)


doMove :: String -> PlayerPos -> IO(Bool, PlayerPos)
doMove direction ppos =
  return (runState (move direction) ppos)

fillOrDecrementWater :: Int -> [[(Bool, String)]] -> (Bool, PlayerPos) -> Int -> IO Int
fillOrDecrementWater currentWater desert newpos maxWater'
  | snd(desert !! fst (snd newpos) !! snd (snd newpos)) == "W" =
      return maxWater'
  | not (fst newpos) = return currentWater
  | otherwise = return (currentWater - 1)

checkTreasureFound :: Int -> String -> IO Int
checkTreasureFound currentTreasures tile =
  if tile == "T" then return (currentTreasures + 1) else return currentTreasures

checkEndGame :: [[(Bool, String)]] ->  PlayerPos -> Int -> IO Int
checkEndGame desert ppos currWater
  | snd(desert !! fst ppos !! snd ppos) `elem` ["L", "L'"] || currWater == 0 = return 1
  | snd(desert !! fst ppos !! snd ppos) == "P" = return 2
  | otherwise = return 0

pickUpTreasure :: [[(Bool, String)]] -> PlayerPos -> IO [[(Bool, String)]]
pickUpTreasure desert ppos = do
  let d = replaceAt ppos desert "D"
  return d

returnDesert :: [[(Bool, String)]] -> IO [[(Bool, String)]]
returnDesert = return

drawPlayerPos :: PlayerPos -> IO()
drawPlayerPos pos = putStr ("(" ++ show (fst pos) ++ "," ++ show(snd pos) ++ ")")


move :: String -> State PlayerPos Bool
move d = state $ \(row, col) -> case d of
  "w" -> if row > 0 then (True, (row-1, col)) else (False, (row, col))
  "s" -> (True, (row+1, col))
  "d" -> (True, (row, col+1))
  "a" -> if col > 0 then (True, (row, col-1)) else (False, (row, col))
  _ -> (False, (row, col))


bfs :: [[(Bool, String)]] -> (PlayerPos, Int) -> String -> [(PlayerPos, Int)] -> [(PlayerPos, Int)] -> Int
bfs desert (ppos, dist) value queue marked =
  let adj = getAdj desert (ppos, dist) marked
   in let result = checkTermination desert adj value
   in
   if fst result
     then snd result
     else
       if null queue
         then
           let queue' = [] ++ adj
            in bfs desert (head queue') value queue' (marked ++ [head queue'])
          else
            let queue' = tail queue ++ adj
              in bfs desert (head queue') value queue' (marked ++ [head queue'])


getAdj :: [[(Bool, String)]] -> (PlayerPos, Int) -> [(PlayerPos, Int)] -> [(PlayerPos, Int)]
getAdj desert (pos, dist) marked=
  [((row, col), dist + 1) |
  row <- [fst pos + 1 .. fst pos - 1],
  col <- [snd pos + 1 .. snd pos - 1],
  abs((row + col) - uncurry (+) pos) == 1,
  row >= 0 && col >= 0,
  snd (desert!!row!!col) /= "L", (row,col) `notElem` map fst marked]


checkTermination :: [[(Bool, String)]] -> [(PlayerPos, Int)] -> String -> (Bool, Int)
checkTermination desert adj value =
  let x = map (\((row, col), dist) -> (snd(desert!!row!!col) == value, dist)) adj
  in if True `elem` map fst x
    then
      let y = filter fst x
      in head y
      else (False, 0)
