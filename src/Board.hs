module Board (Board, initialBoard, movePiece, isWinningState, isDrawState, Square(..)) where

import Data.Array
import Piece

data Square = Square Piece | Empty
            deriving (Show, Eq)

type Coordinates = (Int, Int)
type Board = Array Coordinates Square

initialBoard :: Board
initialBoard = listArray ((0, 0), (3, 3)) (repeat Empty)

movePiece :: Board -> Piece -> Coordinates -> Maybe Board
movePiece = undefined

isWinningState :: Board -> Bool
isWinningState = undefined

isDrawState :: Board -> Bool
isDrawState = undefined
