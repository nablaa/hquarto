module Main (main) where

import Data.Char
import Data.Maybe
import Piece
import Game
import Board

main :: IO ()
main = firstMove >> putStrLn ""

firstMove :: IO GameState
firstMove = do putStrLn "Piece to place: "
               pieceStr <- getLine
               case readPiece pieceStr of
                       Just piece -> playGame (newGame piece)
                       Nothing -> putStrLn "Invalid piece" >> firstMove

playGame :: GameState -> IO GameState
playGame state | isJust (getWinner state) = putStrLn (printGameState state) >> return state
               | otherwise = do putStrLn $ printGameState state
                                putStrLn movePrompt
                                moveStr <- getLine
                                case parseMove moveStr of
                                        Just (coordinates, piece) -> case makeMove state coordinates piece of
                                                                             Just newState -> playGame newState
                                                                             Nothing -> putStrLn invalidMove >> playGame state
                                        Nothing -> putStrLn invalidMove >> playGame state

        where movePrompt = "\nEnter move: "
              invalidMove = "\nInvalid Move!\n\n"

parseMove :: String -> Maybe (Coordinates, Piece)
parseMove str = if length parts /= 3
                then Nothing
                else do x <- parseCoordinate x_str
                        y <- parseCoordinate y_str
                        piece <- readPiece piece_str
                        Just ((x, y), piece)
        where parts = words str
              x_str = head parts
              y_str = parts !! 1
              piece_str = parts !! 2

parseCoordinate :: String -> Maybe Int
parseCoordinate [x] = Just $ ord x - ord '0'
parseCoordinate _ = Nothing
