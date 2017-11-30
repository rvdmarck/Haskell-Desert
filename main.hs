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
infiniteRandomLists = map (randomRs (0,100)) . infiniteGenerators

infiniteRandomList :: (RandomGen g, Random a, Num a) => g -> [(a,g)]
infiniteRandomList = map (randomR (0,100)) . infiniteGenerators

main :: IO b
main = do
  let ppos = (0,0)
  --params <- paramsLoop
  let params = Params 1 7 1 50 10 5 10 10
  let tileList = initTileList params
  let randList = map fst (infiniteRandomList (mkStdGen (initialSeed params)))::[Int]
  print (take 20 randList)
  let desert = infiniteRandomLists (mkStdGen (initialSeed params))
  let desert' = (map . map) (corresp tileList params) desert
  --let desert'' = (map  map) (fillTreasures )
  gameLoop ppos desert' params (maxWater params)


corresp :: [String] -> Params -> Int -> String
corresp tileList params proba
  | tileList !! proba /= "D" = tileList !! proba
  | proba <= treasurelh params = "T"
  | otherwise = "D"



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
  mapM_ print (makeSubMatrix' d startRow endRow startCol endCol)
  return()

makeSubMatrix' :: [[String]] -> Int -> Int -> Int -> Int -> [[String]]
makeSubMatrix' desert startRow endRow startCol endCol
  | startRow /= endRow = take (endCol - startCol) (drop startCol (desert !! startRow) ) : makeSubMatrix' desert (startRow + 1) endRow startCol endCol
  | startRow == endRow = []

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




gameLoop :: PlayerPos -> Sdesert -> Params -> Int -> IO b
gameLoop ppos desert params currentWater = do
  printMatrix' desert 0 10 0 10 ppos
  putStrLn "w,a,s,d : "
  input <- getLine
  newpos <- doMove input ppos
  draw newpos
  currentWater' <- fillOrDecrementWater currentWater (desert !! fst(snd newpos) !! snd (snd newpos)) (maxWater params)
  print currentWater'
  gameLoop (snd newpos) desert params currentWater'

doMove :: String -> PlayerPos -> IO((), PlayerPos)
doMove direction ppos =
  return (runState (move direction) ppos)

fillOrDecrementWater :: Int -> String -> Int -> IO Int
fillOrDecrementWater currentWater tile maxWater' =
  if tile == "W" then return maxWater' else return (currentWater-1)


draw :: ((), PlayerPos) -> IO()
draw pos = putStrLn ("(" ++ show (fst(snd pos)) ++ "," ++ show(snd(snd pos)) ++ ")")

move :: String -> State PlayerPos ()
move d = state $ \(row, col) -> case d of
  "w" -> if row > 0 then ((), (row-1, col)) else ((), (row, col))
  "s" -> ((), (row+1, col))
  "d" -> ((), (row, col+1))
  "a" -> if col > 0 then ((), (row, col-1)) else ((), (row, col))
  _ -> ((), (row, col))
