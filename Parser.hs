module Parser
(
    gameParser
,   ParseInfos (..)
) where

import Text.ParserCombinators.Parsec
import MapGeneration
import Desert

type Coordinate = (Int, Int)

data ParseInfos = ParseInfos
            {     parsedPlayerPos :: Coordinate
                , parsedSupply :: Int
                , parsedRevealed :: [Coordinate]
                , parsedCollected :: [Coordinate]
                , parsedParams :: Params
                , parsedWorms :: [Worm]
            } deriving(Show)

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"


minusParser :: Parser String
minusParser = many1 lower

intParser :: Parser Int
intParser = fmap read (many1 digit)


singleCoordParensParser :: Parser Coordinate
singleCoordParensParser = do
    char '('
    space
    coord <- singleCoordParser
    space
    char ')'
    return coord

singleCoordParser :: Parser Coordinate
singleCoordParser = do
    char '['
    space
    x <- intParser
    space
    char ','
    space
    y <- intParser
    space
    char ']'
    return (x,y)

manyCoordParser :: Parser [Coordinate]
manyCoordParser = do
    char '('
    space
    coords <- sepBy singleCoordParser (try (string " , "))
    space
    char ')'
    return coords


parensParser :: Parser Int
parensParser = do 
    char '('
    space
    val <- intParser
    space
    char ')'
    return val

playerPositionParser :: Parser Coordinate
playerPositionParser = do
    minusParser
    space
    coord <- singleCoordParensParser
    return coord

paramParser :: Parser Int
paramParser = do
    minusParser
    space
    val <- parensParser
    return val 

coordParser :: Parser String -> Parser Coordinate
coordParser parserString = do
    parserString
    coord <- singleCoordParensParser
    eol
    return coord

coordsParser :: Parser String -> Parser [Coordinate]
coordsParser parserString = do
    parserString
    coords <- manyCoordParser
    eol 
    return coords


gameParser :: Parser ParseInfos
gameParser = do
    playerPos <- playerPositionParser
    eol
    supply <- paramParser
    eol
    revealed <- many (coordParser (string "revealed "))
    collected <- many (coordParser (string "collected "))
    emerging <- many (coordsParser (string "emerging "))
    disappearing <- many (coordsParser (string "disappearing "))
    s <- paramParser
    eol
    m <- paramParser
    eol
    g <- paramParser
    eol
    t <- paramParser
    eol
    w <- paramParser
    eol
    p <- paramParser
    eol
    l <- paramParser
    eol
    ll <- paramParser
    eol
    x <- paramParser
    eol
    y <- paramParser
    eol
    return (ParseInfos playerPos supply revealed collected (Params s m g t w p l ll x y) (makeWorms emerging disappearing))


parseGame :: String -> Either ParseError ParseInfos
parseGame = parse gameParser "(unknown)" 

makeWorms :: [[Coordinate]] -> [[Coordinate]] -> [Worm]
makeWorms emerging disappearing = 
    map (makeWorm True) emerging ++ map (makeWorm False) disappearing

makeWorm :: Bool -> [Coordinate] -> Worm
makeWorm emerging coords =
    Worm coords emerging