import System.Environment
import System.Random
import Control.Monad.State

data Tile = DESERT | LAVA | TREASURE | WATER
            deriving (Show, Read)

data GameState = GameState
  { playerPos :: Maybe PlayerPos
  , waterSupplies :: Int
  , treasures :: Int
  , generator :: IO StdGen}

type Desert = [[Tile]]
type PlayerPos = (Int, Int)

main :: IO b
main = do
  (command:args) <- getArgs
  seed <- newStdGen
  let game = GameState (Just(0,0)) 0 0 newStdGen
  let ppos = (0,0)
  gameLoop ppos


randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random


gameLoop :: PlayerPos -> IO b
gameLoop ppos = do
  putStrLn "w,a,s,d"
  input <- getLine
  case input of
    "w" -> doMove "w"
  newpos <- doMove' ppos
  draw' newpos
  gameLoop (snd newpos)

doMove :: String -> IO(State GameState(Maybe PlayerPos))
doMove "w" =
  return (move_ "w")

move_ :: String -> State GameState (Maybe PlayerPos)
move_ "w" = do
  currentState <- get
  let newPos = moveUp_ (playerPos currentState)
  put (currentState {playerPos = newPos})
  return newPos

doMove' :: PlayerPos -> IO((), PlayerPos)
doMove' ppos =
  return (runState test ppos)

test :: State PlayerPos ()
test = state $ \(a,b) -> ((),(a+1, b))

draw' :: ((), PlayerPos) -> IO()
draw' pos = putStrLn ("(" ++ show (fst(snd pos)) ++ "," ++ show(snd(snd pos)) ++ ")")



moveUp_ :: Maybe PlayerPos -> Maybe PlayerPos
moveUp_ (Just(x,y)) = Just(x-1, y)

draw :: Maybe PlayerPos -> IO()
draw Nothing = putStrLn "Wrong input"
draw (Just pos) = putStrLn ("(" ++ show (fst pos) ++ "," ++ show(snd pos) ++ ")")

move :: String -> IO (Maybe PlayerPos)
move "w" =
  return (Just (moveUp (0,0)))
move "a" =
  return (Just (moveLeft (0,0)))
move "s" =
  return (Just (moveDown (0,0)))
move "d" =
  return (Just (moveRight (0,0)))
move _ =
  return Nothing

moveUp :: PlayerPos -> PlayerPos
moveUp (x,y) = (x-1, y)

moveLeft :: PlayerPos -> PlayerPos
moveLeft (x,y) = (x, y-1)

moveDown :: PlayerPos -> PlayerPos
moveDown (x,y) = (x+1, y)

moveRight :: PlayerPos -> PlayerPos
moveRight (x,y) = (x, y+1)
