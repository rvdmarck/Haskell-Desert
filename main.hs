import System.Environment()
import System.Random
import Control.Monad.State
import Data.List
import Text.Read

data Tile = DESERT | LAVA | TREASURE | WATER
            deriving (Show, Read)


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
infiniteRandomLists = map (randomRs (0,100)) . infiniteGenerators

main :: IO b
main = do
  let ppos = (0,0)
  --params <- paramsLoop
  let params = Params 1 1 1 5 10 5 10 10
  let tileList = initTileList params
  let desert = infiniteRandomLists (mkStdGen (initialSeed params))::Desert
  let desert' = (map . map) (corresp tileList) desert
  printMatrix' desert' 0 10 0 10 ppos
  --printMatrix desert 0 10 0 10
  gameLoop ppos desert'

corresp :: [String] -> Int -> String
corresp tileList proba
  | tileList !! proba /= "L'" = tileList !! proba
  | tileList !! proba == "L'" = tileList !! proba

initTileList :: Params -> [String]
initTileList params =
  let tmpList = replicate (treasurelh params) "T" ++ replicate (waterlh params) "W" ++ replicate (portallh params) "P" ++ replicate (lavalh params) "L" ++ replicate (lavalh' params) "L'"
  in tmpList ++ replicate (100 - length tmpList) "D"


printMatrix :: Desert -> Int -> Int -> Int -> Int -> IO()
printMatrix desert startRow endRow startCol endCol = do
  mapM_ print (makeSubMatrix desert startRow endRow startCol endCol)
  return()

makeSubMatrix :: Desert -> Int -> Int -> Int -> Int -> [[Int]]
makeSubMatrix desert startRow endRow startCol endCol
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix desert (startRow + 1) endRow startCol endCol
  | startRow == endRow = []

placePlayer :: [[String]] -> PlayerPos -> [[String]]
placePlayer desert ppos =
  let desert =

printMatrix' :: [[String]] -> Int -> Int -> Int -> Int -> PlayerPos -> IO()
printMatrix' desert startRow endRow startCol endCol ppos = do
  print (desert !! fst ppos !! snd ppos)
  --let desert !! (fst ppos) !! (snd ppos) = "P" in
  mapM_ print (makeSubMatrix' desert startRow endRow startCol endCol)
  return()

makeSubMatrix' :: [[String]] -> Int -> Int -> Int -> Int -> [[String]]
makeSubMatrix' desert startRow endRow startCol endCol
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix' desert (startRow + 1) endRow startCol endCol
  | startRow == endRow = []



randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

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
      putStrLn "\n ==== \nWrong paramter input"
      paramsLoop
    else do
      let params' = Params (read s) (read m) (read g) (read t) (read w) (read p) (read l) (read ll)
      print params'
      return params'




gameLoop :: PlayerPos -> Sdesert -> IO b
gameLoop ppos desert = do
  printMatrix' desert 0 10 0 10 ppos
  putStrLn "w,a,s,d : "
  input <- getLine
  newpos <- doMove input ppos
  draw newpos
  gameLoop (snd newpos) desert

doMove :: String -> PlayerPos -> IO((), PlayerPos)
doMove direction ppos =
  return (runState (move direction) ppos)

draw :: ((), PlayerPos) -> IO()
draw pos = putStrLn ("(" ++ show (fst(snd pos)) ++ "," ++ show(snd(snd pos)) ++ ")")

move :: String -> State PlayerPos ()
move d = state $ \(row, col) -> case d of
  "w" -> ((), (row-1, col))
  "s" -> ((), (row+1, col))
  "d" -> ((), (row, col+1))
  "a" -> ((), (row, col-1))
  _ -> ((), (row, col))
