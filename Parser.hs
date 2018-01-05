module Parser
(
    gameParser
,   ParseInfos (..)
) where

import Text.ParserCombinators.Parsec
import MapGeneration

type Coordinate = (Int, Int)

data ParseInfos = ParseInfos
            {     parsedPlayerPos :: Coordinate
                , parsedSupply :: Int
                , parsedRevealed :: [Coordinate]
                , parsedCollected :: [Coordinate]
                , parsedParams :: Params
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


coordParser :: Parser (Int, Int)
coordParser = do
    oneOf "("
    oneOf "["
    x <- intParser
    oneOf ","
    y <- intParser
    oneOf "]"
    oneOf ")"
    return (x,y)

parensParser :: Parser Int
parensParser = do 
    oneOf "("
    val <- intParser
    oneOf ")"
    return val

playerPositionParser :: Parser (Int, Int)
playerPositionParser = do
    minusParser
    space
    coord <- coordParser
    return coord

paramParser :: Parser Int
paramParser = do
    minusParser
    space
    val <- parensParser
    return val 

revealedStringParser :: Parser String
revealedStringParser = string "revealed"

collectedStringParser :: Parser String
collectedStringParser = string "collected"

coordsParser :: Parser String ->Parser (Int, Int)
coordsParser parserString = do
    parserString
    space 
    coord <- coordParser
    eol
    return coord


gameParser :: Parser ParseInfos
gameParser = do
    playerPos <- playerPositionParser
    eol
    supply <- paramParser
    eol
    revealed <- many (coordsParser revealedStringParser)
    collected <- many (coordsParser collectedStringParser)
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
    return (ParseInfos playerPos supply revealed collected (Params s m g t w p l ll y))


parseGame :: String -> Either ParseError ParseInfos
parseGame = parse gameParser "(unknown)" 

--parseGameFromFile :: Parser ParseInfos -> String -> IO (Either ParseError ParseInfos)
--parseGameFromFile = parseFromFile gameParser

doParse = 
    parseGame "position ([0,0])\nsupply (50)\nrevealed ([0,0])\nrevealed ([1,0])\ncollected ([0,0])\ncollected ([1,0])\ns (2)\nm (3)\ng (13)\nt (10)\nw (25)\np (15)\nl (15)\nll (20)\nx (3)\ny (5)\n"