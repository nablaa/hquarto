module Game (GameState(..), newGame, makeMove, getWinner,
             isDraw, hasEnded, nextPlayer) where

import Data.Maybe
import Piece
import Board

data Player = First | Second
            deriving (Eq, Show)

data GameState = GameState {
               gameBoard :: Board,
               gamePlayer :: Player,
               gamePieceToPlay :: Piece
               } deriving (Eq, Show)

newGame :: Piece -> GameState
newGame = GameState initialBoard First

makeMove :: GameState -> Coordinates -> Piece -> Maybe GameState
makeMove (GameState board player piece) coordinates nextPiece =
        case movePiece board piece coordinates of
                Nothing -> Nothing
                Just newBoard -> if isLegalNextPiece newBoard nextPiece
                                 then Just $ GameState newBoard (nextPlayer player) nextPiece
                                 else Nothing


getWinner :: GameState -> Maybe Player
getWinner (GameState board player _) = if isWinningState board
                                       then Just $ nextPlayer player
                                       else Nothing

isDraw :: GameState -> Bool
isDraw = isDrawState . gameBoard

hasEnded :: GameState -> Bool
hasEnded game = isDraw game || isJust (getWinner game)

nextPlayer :: Player -> Player
nextPlayer First = Second
nextPlayer Second = First
