module Events
(
    getNumEvent
,   getAlphaEvent
)where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G


getNumEvent :: Event -> String
getNumEvent event 
    | EventKey (G.Char '0') Down _ _ <- event = "0"
    | EventKey (G.Char '1') Down _ _ <- event = "1"
    | EventKey (G.Char '2') Down _ _ <- event = "2"
    | EventKey (G.Char '3') Down _ _ <- event = "3"
    | EventKey (G.Char '4') Down _ _ <- event = "4"
    | EventKey (G.Char '5') Down _ _ <- event = "5"
    | EventKey (G.Char '6') Down _ _ <- event = "6"
    | EventKey (G.Char '7') Down _ _ <- event = "7"
    | EventKey (G.Char '8') Down _ _ <- event = "8"
    | EventKey (G.Char '9') Down _ _ <- event = "9"
    | EventKey (SpecialKey KeyEnter) Down _ _ <- event = "enter"
    | otherwise = "undefined"

getAlphaEvent :: Event -> String 
getAlphaEvent event
    | EventKey (G.Char 'a') Down _ _ <- event = "a"
    | EventKey (G.Char 'b') Down _ _ <- event = "b"
    | EventKey (G.Char 'c') Down _ _ <- event = "c"
    | EventKey (G.Char 'd') Down _ _ <- event = "d"
    | EventKey (G.Char 'e') Down _ _ <- event = "e"
    | EventKey (G.Char 'f') Down _ _ <- event = "f"
    | EventKey (G.Char 'g') Down _ _ <- event = "g"
    | EventKey (G.Char 'h') Down _ _ <- event = "h"
    | EventKey (G.Char 'i') Down _ _ <- event = "i"
    | EventKey (G.Char 'j') Down _ _ <- event = "j"
    | EventKey (G.Char 'k') Down _ _ <- event = "k"
    | EventKey (G.Char 'l') Down _ _ <- event = "l"
    | EventKey (G.Char 'm') Down _ _ <- event = "m"
    | EventKey (G.Char 'n') Down _ _ <- event = "n"
    | EventKey (G.Char 'o') Down _ _ <- event = "o"
    | EventKey (G.Char 'p') Down _ _ <- event = "p"
    | EventKey (G.Char 'q') Down _ _ <- event = "q"
    | EventKey (G.Char 'r') Down _ _ <- event = "r"
    | EventKey (G.Char 's') Down _ _ <- event = "s"
    | EventKey (G.Char 't') Down _ _ <- event = "t"
    | EventKey (G.Char 'u') Down _ _ <- event = "u"
    | EventKey (G.Char 'v') Down _ _ <- event = "v"
    | EventKey (G.Char 'w') Down _ _ <- event = "w"
    | EventKey (G.Char 'x') Down _ _ <- event = "x"
    | EventKey (G.Char 'y') Down _ _ <- event = "y"
    | EventKey (G.Char 'z') Down _ _ <- event = "z"
    | EventKey (G.Char 'A') Down _ _ <- event = "A"
    | EventKey (G.Char 'B') Down _ _ <- event = "B"
    | EventKey (G.Char 'C') Down _ _ <- event = "C"
    | EventKey (G.Char 'D') Down _ _ <- event = "D"
    | EventKey (G.Char 'E') Down _ _ <- event = "E"
    | EventKey (G.Char 'F') Down _ _ <- event = "F"
    | EventKey (G.Char 'G') Down _ _ <- event = "G"
    | EventKey (G.Char 'H') Down _ _ <- event = "H"
    | EventKey (G.Char 'I') Down _ _ <- event = "I"
    | EventKey (G.Char 'J') Down _ _ <- event = "J"
    | EventKey (G.Char 'K') Down _ _ <- event = "K"
    | EventKey (G.Char 'L') Down _ _ <- event = "L"
    | EventKey (G.Char 'M') Down _ _ <- event = "M"
    | EventKey (G.Char 'N') Down _ _ <- event = "N"
    | EventKey (G.Char 'O') Down _ _ <- event = "O"
    | EventKey (G.Char 'P') Down _ _ <- event = "P"
    | EventKey (G.Char 'Q') Down _ _ <- event = "Q"
    | EventKey (G.Char 'R') Down _ _ <- event = "R"
    | EventKey (G.Char 'S') Down _ _ <- event = "S"
    | EventKey (G.Char 'T') Down _ _ <- event = "T"
    | EventKey (G.Char 'U') Down _ _ <- event = "U"
    | EventKey (G.Char 'V') Down _ _ <- event = "V"
    | EventKey (G.Char 'W') Down _ _ <- event = "W"
    | EventKey (G.Char 'X') Down _ _ <- event = "X"
    | EventKey (G.Char 'Y') Down _ _ <- event = "Y"
    | EventKey (G.Char 'Z') Down _ _ <- event = "Z"
    | EventKey (G.Char '.') Down _ _ <- event = "."
    | EventKey (SpecialKey KeyEnter) Down _ _ <- event = "enter"
    | otherwise = getNumEvent event