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
movePiece = undefined

isWinningState :: Board -> Bool
isWinningState = undefined

isDrawState :: Board -> Bool
isDrawState = undefined

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 3 && j >= 0 && j <= 3

isPieceOnBoard :: Board -> Piece -> Bool
isPieceOnBoard board piece = Cell piece `elem` elems board

setPiece :: Board -> Coordinates -> Piece -> Board
setPiece board coordinates piece = board // [(coordinates, Cell piece)]
