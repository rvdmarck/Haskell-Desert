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
  let params = Params 1 7 1 10 30 20 0 0
  let tileList = initTileList params
  let randomMat = infiniteRandomLists (mkStdGen (initialSeed params))
  let randomTiles = (map . map) (corresp tileList params) randomMat

  let randomTreasures = infiniteRandomLists (mkStdGen (initialSeed params * 2))
  let randomTreasuresTiles = (map . map) (corresp' params) randomTreasures
  let desert = zipWith (zipWith compareTreasure) randomTiles randomTreasuresTiles
  gameLoop ppos desert params (maxWater params) 0


corresp :: [String] -> Params -> Int -> String
corresp tileList params proba
  | tileList !! proba /= "D" = tileList !! proba
  | otherwise = "D"

corresp' :: Params -> Int -> String
corresp' params proba
  | proba <= treasurelh params = "T"
  | otherwise = "D"

compareTreasure :: String -> String -> String
compareTreasure x y  =
  if x == "D"
    then
      if y =="T"
        then "T"
        else "D"
    else x

randomSt :: (RandomGen g, Random a, Num a) => State g a
randomSt = state (randomR (0,100))

getRandom :: State StdGen Int
getRandom = randomSt

initTileList :: Params -> [String]
initTileList params =
  --let tmpList = replicate (treasurelh params) "T" ++ replicate (waterlh params) "W" ++ replicate (portallh params) "P" ++ replicate (lavalh params) "L" ++ replicate (lavalh' params) "L'"
  let tmpList = replicate (waterlh params) "W" ++ replicate (portallh params) "P" ++ replicate (lavalh params) "L" ++ replicate (lavalh' params) "L'"
    in tmpList ++ replicate (100 - length tmpList) "D"

{--
printMatrix :: Desert -> Int -> Int -> Int -> Int -> IO()
printMatrix desert startRow endRow startCol endCol = do
  mapM_ print (makeSubMatrix desert startRow endRow startCol endCol)
  return()

makeSubMatrix :: Desert -> Int -> Int -> Int -> Int -> [[Int]]
makeSubMatrix desert startRow endRow startCol endCol
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix desert (startRow + 1) endRow startCol endCol
  | startRow == endRow = []

--}
printMatrix' :: [[String]] -> Int -> Int -> Int -> Int -> PlayerPos -> IO()
printMatrix' desert startRow endRow startCol endCol ppos = do
  let d = replaceAt ppos desert "Pl"
  if startRow < 0
    then
      if startCol < 0
        then mapM_ printDesertLine (makeSubMatrix' d (max 0 startRow) (endRow + abs startRow) (max 0 startCol) (endCol + abs startCol))
        else mapM_ printDesertLine (makeSubMatrix' d (max 0 startRow) (endRow + abs startRow) (max 0 startCol) endCol)
    else
      if startCol < 0
        then mapM_ printDesertLine (makeSubMatrix' d (max 0 startRow) endRow (max 0 startCol) (endCol + abs startCol))
        else mapM_ printDesertLine (makeSubMatrix' d (max 0 startRow) endRow (max 0 startCol) endCol)
  return()

makeSubMatrix' :: [[String]] -> Int -> Int -> Int -> Int -> [[String]]
makeSubMatrix' desert startRow endRow startCol endCol
  | startRow == endRow = []
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix' desert (startRow + 1) endRow startCol endCol

printDesertLine :: [String] -> IO()
printDesertLine l= print (unwords l)


replaceAt :: PlayerPos -> [[String]] -> String -> [[String]]
replaceAt ppos desert val =
  let (x,_:ys) = splitAt (snd ppos) (desert !! fst ppos)
    in let (x',_ : ys') = splitAt (fst ppos) desert
      in x' ++ [x ++ val : ys] ++ ys'

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




gameLoop :: PlayerPos -> Sdesert -> Params -> Int -> Int -> IO ()
gameLoop ppos desert params currentWater currentTreasures = do
  printMatrix' desert (fst ppos - 5) (fst ppos + 6) (snd ppos - 5) (snd ppos + 6) ppos
  putStrLn "w,a,s,d : "
  input <- getLine
  newpos <- doMove input ppos
  draw newpos
  currentWater' <- fillOrDecrementWater currentWater desert newpos (maxWater params)
  endGame <- checkEndGame desert (snd newpos) currentWater'
  currentTreasures' <- checkTreasureFound currentTreasures (desert !! fst(snd newpos) !! snd (snd newpos))
  print currentWater'
  print currentTreasures'
  if currentTreasures' /= currentTreasures
    then do
      desert' <- pickUpTreasure desert (snd newpos)
      _ <- return desert'
      if endGame == 0
        then gameLoop (snd newpos) desert' params currentWater' currentTreasures'
        else
          if endGame == 1
            then putStrLn "You're DEAD !"
            else
              when  (endGame == 2) $
                putStrLn "You WON !"


    else do
      desert' <- returnDesert desert
      _ <- return desert'
      if endGame == 0
        then gameLoop (snd newpos) desert' params currentWater' currentTreasures'
        else
          if endGame == 1
            then putStrLn "You're DEAD !"
            else
              when  (endGame == 2) $
                putStrLn "You WON !"

getLos :: PlayerPos -> Int -> [PlayerPos]
getLos ppos los =
  let xs = [-los..los]
    in [(x + fst ppos, y + snd ppos) | x <- xs, y <- xs, abs x + abs y <= los]



doMove :: String -> PlayerPos -> IO(Bool, PlayerPos)
doMove direction ppos =
  return (runState (move direction) ppos)

fillOrDecrementWater :: Int -> [[String]] -> (Bool, PlayerPos) -> Int -> IO Int
fillOrDecrementWater currentWater desert newpos maxWater'
  | (desert !! fst (snd newpos) !! snd (snd newpos)) == "W" =
      return maxWater'
  | not (fst newpos) = return currentWater
  | otherwise = return (currentWater - 1)

checkTreasureFound :: Int -> String -> IO Int
checkTreasureFound currentTreasures tile =
  if tile == "T" then return (currentTreasures + 1) else return currentTreasures

checkEndGame :: [[String]] ->  PlayerPos -> Int -> IO Int
checkEndGame desert ppos currWater
  | desert !! fst ppos !! snd ppos `elem` ["L", "L'"] || currWater == 0 = return 1
  | desert !! fst ppos !! snd ppos == "P" = return 2
  | otherwise = return 0

pickUpTreasure :: [[String]] -> PlayerPos -> IO [[String]]
pickUpTreasure desert ppos = do
  let d = replaceAt ppos desert "D"
  return d

returnDesert :: [[String]] -> IO [[String]]
returnDesert = return

draw :: (Bool, PlayerPos) -> IO()
draw pos = putStrLn ("(" ++ show (fst(snd pos)) ++ "," ++ show(snd(snd pos)) ++ ")")

move :: String -> State PlayerPos Bool
move d = state $ \(row, col) -> case d of
  "w" -> if row > 0 then (True, (row-1, col)) else (False, (row, col))
  "s" -> (True, (row+1, col))
  "d" -> (True, (row, col+1))
  "a" -> if col > 0 then (True, (row, col-1)) else (False, (row, col))
  _ -> (False, (row, col))
