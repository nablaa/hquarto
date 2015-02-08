module Board (Board, initialBoard, movePiece,
              isWinningState, isDrawState, Cell(..),
              isInsideBoard, isPieceOnBoard,
              setPiece) where

import Data.Array
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
isWinningState = undefined

isDrawState :: Board -> Bool
isDrawState = undefined

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 3 && j >= 0 && j <= 3

isEmptyCell :: Board -> Coordinates -> Bool
isEmptyCell board coordinates = board ! coordinates == Empty

isPieceOnBoard :: Board -> Piece -> Bool
isPieceOnBoard board piece = Cell piece `elem` elems board

setPiece :: Board -> Coordinates -> Piece -> Board
setPiece board coordinates piece = board // [(coordinates, Cell piece)]
