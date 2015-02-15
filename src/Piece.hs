module Piece (Piece(..), Color(..), Shape(..),
              Height(..), Top(..), haveCommonProperty,
              printPiece, readPiece) where

import Data.Char
import Data.List

data Color = White | Black
           deriving (Show, Eq)

data Shape = Circular | Square
           deriving (Show, Eq)

data Height = Short | Tall
            deriving (Show, Eq)

data Top = Hollow | Solid
         deriving (Show, Eq)

data Piece = Piece Color Shape Height Top
           deriving (Show, Eq)

haveCommonProperty :: [Piece] -> Bool
haveCommonProperty pieces = any (haveCommonBool pieces) [0..3]

haveCommonBool :: [Piece] -> Int -> Bool
haveCommonBool pieces index = length (nub (map ((!! index) . pieceToBools) pieces)) == 1

pieceToBools :: Piece -> [Bool]
pieceToBools (Piece White Circular Short Hollow) = [False, False, False, False]
pieceToBools (Piece White Circular Short Solid) = [False, False, False, True]
pieceToBools (Piece White Circular Tall Hollow) = [False, False, True, False]
pieceToBools (Piece White Circular Tall Solid) = [False, False, True, True]
pieceToBools (Piece White Square Short Hollow) = [False, True, False, False]
pieceToBools (Piece White Square Short Solid) = [False, True, False, True]
pieceToBools (Piece White Square Tall Hollow) = [False, True, True, False]
pieceToBools (Piece White Square Tall Solid) = [False, True, True, True]
pieceToBools (Piece Black Circular Short Hollow) = [True, False, False, False]
pieceToBools (Piece Black Circular Short Solid) = [True, False, False, True]
pieceToBools (Piece Black Circular Tall Hollow) = [True, False, True, False]
pieceToBools (Piece Black Circular Tall Solid) = [True, False, True, True]
pieceToBools (Piece Black Square Short Hollow) = [True, True, False, False]
pieceToBools (Piece Black Square Short Solid) = [True, True, False, True]
pieceToBools (Piece Black Square Tall Hollow) = [True, True, True, False]
pieceToBools (Piece Black Square Tall Solid) = [True, True, True, True]

printPiece :: Piece -> String
printPiece piece = zipWith caseSelector (pieceToBools piece) "CSHT"
        where caseSelector True = toUpper
              caseSelector False = toLower

readPiece :: String -> Maybe Piece
readPiece [c, s, h, t] = do color <- readColor c
                            shape <- readShape s
                            height <- readHeight h
                            top <- readTop t
                            Just (Piece color shape height top)
readPiece _ = Nothing

readColor :: Char -> Maybe Color
readColor 'c' = Just White
readColor 'C' = Just Black
readColor _ = Nothing

readShape :: Char -> Maybe Shape
readShape 's' = Just Circular
readShape 'S' = Just Square
readShape _ = Nothing

readHeight :: Char -> Maybe Height
readHeight 'h' = Just Short
readHeight 'H' = Just Tall
readHeight _ = Nothing

readTop :: Char -> Maybe Top
readTop 't' = Just Hollow
readTop 'T' = Just Solid
readTop _ = Nothing
