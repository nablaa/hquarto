module Board (Board, initialBoard, movePiece,
              isWinningState, isDrawState, Cell(..),
              isInsideBoard, isPieceOnBoard,
              setPiece, isValidBoard) where

import Data.Maybe
import Data.Array
import Data.List
import Piece

data Cell = Cell Piece | Empty
            deriving (Show, Eq)

type Coordinates = (Int, Int)
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

setPiece :: Board -> Coordinates -> Piece -> Board
setPiece board coordinates piece = board // [(coordinates, Cell piece)]

isValidBoard :: Board -> Bool
isValidBoard board = length (nub pieces) == length pieces
        where pieces = filter (Empty /=) (elems board)

boardLines :: [[Coordinates]]
boardLines = [[(i, j) | j <- [0..3]] | i <- [0..3]]
          ++ [[(i, j) | i <- [0..3]] | j <- [0..3]]
          ++ [[(i, j) | i <- [0..3], j <- [0..3], i == j]]
          ++ [[(i, j) | i <- [0..3], j <- [0..3], i + j == 3]]

lineCells:: Board -> [Coordinates] -> [Cell]
lineCells board = map (\x -> board ! x)

linePieces :: Board -> [Coordinates] -> [Piece]
linePieces board line = mapMaybe cellToPiece cells
        where cells = lineCells board line

cellToPiece :: Cell -> Maybe Piece
cellToPiece (Cell piece) = Just piece
cellToPiece Empty = Nothing

isWinningLine :: Board -> [Coordinates] -> Bool
isWinningLine board line = haveCommonProperty pieces
        where pieces = linePieces board line
