module Board (Board, initialBoard, movePiece,
              isWinningState, isDrawState, Cell(..),
              isInsideBoard, isPieceOnBoard,
              setPiece, isValidBoard, Coordinates,
              isLegalNextPiece, printBoard) where

import Data.Maybe
import Data.Array
import Data.List
import Piece

data Cell = Cell Piece | Empty
            deriving (Show, Eq)

type Coordinates = (Int, Int)
type Line = [Coordinates]
type Board = Array Coordinates Cell

initialBoard :: Board
initialBoard = listArray ((0, 0), (3, 3)) (repeat Empty)

movePiece :: Board -> Piece -> Coordinates -> Maybe Board
movePiece board piece coordinates | not (isInsideBoard coordinates) = Nothing
                                  | not (isEmptyCell board coordinates) = Nothing
                                  | isPieceOnBoard board piece = Nothing
                                  | otherwise = Just $ setPiece board coordinates piece


isWinningState :: Board -> Bool
isWinningState board = any (isWinningLine board) boardLines

isDrawState :: Board -> Bool
isDrawState board = not (isWinningState board) && isBoardFull board

isBoardFull :: Board -> Bool
isBoardFull board = Empty `notElem` elems board

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 3 && j >= 0 && j <= 3

isEmptyCell :: Board -> Coordinates -> Bool
isEmptyCell board coordinates = board ! coordinates == Empty

isPieceOnBoard :: Board -> Piece -> Bool
isPieceOnBoard board piece = Cell piece `elem` elems board

isLegalNextPiece :: Board -> Piece -> Bool
isLegalNextPiece board piece = not $ isPieceOnBoard board piece

setPiece :: Board -> Coordinates -> Piece -> Board
setPiece board coordinates piece = board // [(coordinates, Cell piece)]

isValidBoard :: Board -> Bool
isValidBoard board = length (nub pieces) == length pieces
        where pieces = filter (Empty /=) (elems board)

boardLines :: [Line]
boardLines = [[(i, j) | j <- [0..3]] | i <- [0..3]]
          ++ [[(i, j) | i <- [0..3]] | j <- [0..3]]
          ++ [[(i, j) | i <- [0..3], j <- [0..3], i == j]]
          ++ [[(i, j) | i <- [0..3], j <- [0..3], i + j == 3]]

lineCells:: Board -> Line -> [Cell]
lineCells board = map (\x -> board ! x)

linePieces :: Board -> Line -> [Piece]
linePieces board line = mapMaybe cellToPiece cells
        where cells = lineCells board line

cellToPiece :: Cell -> Maybe Piece
cellToPiece (Cell piece) = Just piece
cellToPiece Empty = Nothing

isWinningLine :: Board -> Line -> Bool
isWinningLine board line = length pieces == 4 && haveCommonProperty pieces
        where pieces = linePieces board line

printBoard :: Board -> String
printBoard board = header ++ intercalate header (map printRow rows) ++ header
        where rows = toLines $ elems board :: [[Cell]]
              toLines [] = []
              toLines xs = take 4 xs : toLines (drop 4 xs)
              header = "+----+----+----+----+\n"

printRow :: [Cell] -> String
printRow cells = "|" ++ intercalate "|" (map printCell cells) ++ "|\n"

printCell :: Cell -> String
printCell Empty = "    "
printCell (Cell piece) = printPiece piece
