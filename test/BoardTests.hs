module BoardTests where

import Test.Hspec
import Board
import Piece
import Data.Array
import Data.List

boardSpec :: IO ()
boardSpec = hspec $
        describe "Board" $ do
          initialBoardSpec
          coordinatesSpec
          setPieceSpec
          isPieceOnBoardSpec
          movePieceSpec
          isWinningStateSpec
          isDrawStateSpec
          isValidBoardSpec
          isLegalNextPieceSpec
          printBoardSpec

initialBoardSpec :: Spec
initialBoardSpec =
        describe "initialBoard" $ do
         it "should have only empty squares" $
           nub (elems initialBoard) `shouldBe` [Empty]
         it "should have 4x4 squares" $
           length (elems initialBoard) `shouldBe` (4 * 4)

coordinatesSpec :: Spec
coordinatesSpec =
        describe "isInsideBoard" $ do
          it "coordinates 0,0-3,3 are inside board" $ do
            isInsideBoard (0, 0) `shouldBe` True
            isInsideBoard (3, 3) `shouldBe` True
            isInsideBoard (1, 1) `shouldBe` True
          it "negative and too large coordinates are outside board" $ do
            isInsideBoard (-1, 0) `shouldBe` False
            isInsideBoard (0, -1) `shouldBe` False
            isInsideBoard (4, 3) `shouldBe` False
            isInsideBoard (3, 4) `shouldBe` False

setPieceSpec :: Spec
setPieceSpec =
        describe "setPiece" $
          it "setting piece should update the board" $
            setPiece initialBoard (1, 2) (Piece White Square Tall Solid)
                     `shouldBe`
                     array ((0,0),(3,3))
                           [((0,0),Empty),
                            ((0,1),Empty),
                            ((0,2),Empty),
                            ((0,3),Empty),
                            ((1,0),Empty),
                            ((1,1),Empty),
                            ((1,2),Cell (Piece White Square Tall Solid)),
                            ((1,3),Empty),
                            ((2,0),Empty),
                            ((2,1),Empty),
                            ((2,2),Empty),
                            ((2,3),Empty),
                            ((3,0),Empty),
                            ((3,1),Empty),
                            ((3,2),Empty),
                            ((3,3),Empty)]

isPieceOnBoardSpec :: Spec
isPieceOnBoardSpec =
        describe "isPieceOnBoard" $ do
          it "empty board has no pieces" $
            isPieceOnBoard initialBoard (Piece White Square Tall Solid) `shouldBe` False
          it "no exact piece on board" $
            isPieceOnBoard board (Piece White Square Tall Solid) `shouldBe` False
          it "piece found on board" $
            isPieceOnBoard board (Piece Black Square Tall Solid) `shouldBe` True
        where board = array ((0,0),(3,3))
                           [((0,0),Empty),
                            ((0,1),Empty),
                            ((0,2),Empty),
                            ((0,3),Cell (Piece Black Circular Short Hollow)),
                            ((1,0),Empty),
                            ((1,1),Empty),
                            ((1,2),Cell (Piece Black Square Tall Solid)),
                            ((1,3),Empty),
                            ((2,0),Empty),
                            ((2,1),Empty),
                            ((2,2),Empty),
                            ((2,3),Empty),
                            ((3,0),Empty),
                            ((3,1),Empty),
                            ((3,2),Empty),
                            ((3,3),Empty)]

isLegalNextPieceSpec :: Spec
isLegalNextPieceSpec =
        describe "isLegalNextPiece" $ do
          it "any piece is legal when the board is empty" $ do
            isLegalNextPiece initialBoard (Piece White Square Tall Solid) `shouldBe` True
            isLegalNextPiece initialBoard (Piece Black Circular Short Hollow) `shouldBe` True
          it "previously placed pieces are not legal next pieces" $ do
            isLegalNextPiece board (Piece Black Circular Short Hollow) `shouldBe` False
            isLegalNextPiece board (Piece Black Square Tall Solid) `shouldBe` False
            isLegalNextPiece board (Piece White Square Short Hollow) `shouldBe` False
          it "pieces not on the board are legal next pieces" $ do
            isLegalNextPiece board (Piece Black Circular Short Solid) `shouldBe` True
            isLegalNextPiece board (Piece White Circular Short Solid) `shouldBe` True
        where board = array ((0,0),(3,3))
                           [((0,0),Empty),
                            ((0,1),Empty),
                            ((0,2),Empty),
                            ((0,3),Cell (Piece Black Circular Short Hollow)),
                            ((1,0),Empty),
                            ((1,1),Empty),
                            ((1,2),Cell (Piece Black Square Tall Solid)),
                            ((1,3),Empty),
                            ((2,0),Empty),
                            ((2,1),Empty),
                            ((2,2),Empty),
                            ((2,3),Cell (Piece White Square Short Hollow)),
                            ((3,0),Empty),
                            ((3,1),Empty),
                            ((3,2),Empty),
                            ((3,3),Empty)]


movePieceSpec :: Spec
movePieceSpec =
        describe "movePiece" $ do
          it "move with invalid coordinates should be illegal" $
            movePiece initialBoard (Piece White Square Tall Solid) (-1, 0) `shouldBe` Nothing
          it "move with non-empty cell should be illegal" $
            movePiece board (Piece White Square Tall Solid) (1, 2) `shouldBe` Nothing
          it "move with piece already on board should be illegal" $
            movePiece board (Piece Black Circular Short Hollow) (0, 0) `shouldBe` Nothing
          it "valid move should return updated board" $
            movePiece board (Piece Black Circular Short Solid) (0, 0) `shouldBe` Just newBoard
        where board = array ((0,0),(3,3))
                           [((0,0),Empty),
                            ((0,1),Empty),
                            ((0,2),Empty),
                            ((0,3),Cell (Piece Black Circular Short Hollow)),
                            ((1,0),Empty),
                            ((1,1),Empty),
                            ((1,2),Cell (Piece Black Square Tall Solid)),
                            ((1,3),Empty),
                            ((2,0),Empty),
                            ((2,1),Empty),
                            ((2,2),Empty),
                            ((2,3),Empty),
                            ((3,0),Empty),
                            ((3,1),Empty),
                            ((3,2),Empty),
                            ((3,3),Empty)]
              newBoard = array ((0,0),(3,3))
                           [((0,0),Cell (Piece Black Circular Short Solid)),
                            ((0,1),Empty),
                            ((0,2),Empty),
                            ((0,3),Cell (Piece Black Circular Short Hollow)),
                            ((1,0),Empty),
                            ((1,1),Empty),
                            ((1,2),Cell (Piece Black Square Tall Solid)),
                            ((1,3),Empty),
                            ((2,0),Empty),
                            ((2,1),Empty),
                            ((2,2),Empty),
                            ((2,3),Empty),
                            ((3,0),Empty),
                            ((3,1),Empty),
                            ((3,2),Empty),
                            ((3,3),Empty)]

isWinningStateSpec :: Spec
isWinningStateSpec =
        describe "isWinningState" $ do
          it "empty board is not in win state" $
            isWinningState initialBoard `shouldBe` False
          it "horizontal line of hollows wins" $
            isWinningState horizontalHollows `shouldBe` True
          it "vertical line of blacks wins" $
            isWinningState verticalBlacks `shouldBe` True
          it "diagonal line of talls wins" $
            isWinningState diagonalTalls `shouldBe` True
          it "diagonal line of shorts wins" $
            isWinningState diagonalShorts `shouldBe` True
          it "less than four pieces in line is not a win" $
            isWinningState threeShorts `shouldBe` False


isDrawStateSpec :: Spec
isDrawStateSpec =
        describe "isDrawState" $
          it "horizontal line of hollows is not a draw" $
            isDrawState horizontalHollows `shouldBe` False

isValidBoardSpec :: Spec
isValidBoardSpec =
        describe "isValidBoard" $ do
          it "empty board is valid" $
            isValidBoard initialBoard `shouldBe` True
          it "no two similar pieces means that the board is valid" $
            isValidBoard horizontalHollows `shouldBe` True
          it "two same pieces on board means that the board is invalid" $
            isValidBoard invalidBoard `shouldBe` False
        where invalidBoard = array ((0,0),(3,3))
                                   [
                                   ((0, 0), Empty),
                                   ((0, 1), Empty),
                                   ((0, 2), Empty),
                                   ((0, 3), Empty),
                                   ((1, 0), Cell (Piece White Circular Short Hollow)),
                                   ((1, 1), Cell (Piece White Circular Tall Hollow)),
                                   ((1, 2), Cell (Piece Black Circular Short Hollow)),
                                   ((1, 3), Cell (Piece White Circular Short Hollow)),
                                   ((2, 0), Empty),
                                   ((2, 1), Empty),
                                   ((2, 2), Empty),
                                   ((2, 3), Empty),
                                   ((3, 0), Empty),
                                   ((3, 1), Empty),
                                   ((3, 2), Empty),
                                   ((3, 3), Empty)
                                   ]


horizontalHollows :: Board
horizontalHollows = array ((0,0),(3,3))
        [
        ((0, 0), Empty),
        ((0, 1), Empty),
        ((0, 2), Empty),
        ((0, 3), Empty),
        ((1, 0), Cell (Piece White Circular Short Hollow)),
        ((1, 1), Cell (Piece White Circular Tall Hollow)),
        ((1, 2), Cell (Piece Black Circular Short Hollow)),
        ((1, 3), Cell (Piece White Square Short Hollow)),
        ((2, 0), Empty),
        ((2, 1), Empty),
        ((2, 2), Empty),
        ((2, 3), Empty),
        ((3, 0), Empty),
        ((3, 1), Empty),
        ((3, 2), Empty),
        ((3, 3), Empty)
        ]

verticalBlacks :: Board
verticalBlacks = array ((0,0),(3,3))
        [
        ((0, 0), Empty),
        ((0, 1), Empty),
        ((0, 2), Cell (Piece Black Circular Short Hollow)),
        ((0, 3), Empty),
        ((1, 0), Empty),
        ((1, 1), Empty),
        ((1, 2), Cell (Piece Black Square Tall Solid)),
        ((1, 3), Empty),
        ((2, 0), Empty),
        ((2, 1), Empty),
        ((2, 2), Cell (Piece Black Square Short Solid)),
        ((2, 3), Empty),
        ((3, 0), Empty),
        ((3, 1), Empty),
        ((3, 2), Cell (Piece Black Square Short Hollow)),
        ((3, 3), Empty)
        ]

diagonalTalls :: Board
diagonalTalls = array ((0,0),(3,3))
        [
        ((0, 0), Cell (Piece White Circular Tall Solid)),
        ((0, 1), Empty),
        ((0, 2), Empty),
        ((0, 3), Empty),
        ((1, 0), Empty),
        ((1, 1), Cell (Piece Black Circular Tall Solid)),
        ((1, 2), Empty),
        ((1, 3), Empty),
        ((2, 0), Empty),
        ((2, 1), Empty),
        ((2, 2), Cell (Piece Black Square Tall Hollow)),
        ((2, 3), Empty),
        ((3, 0), Empty),
        ((3, 1), Empty),
        ((3, 2), Empty),
        ((3, 3), Cell (Piece White Square Tall Hollow))
        ]

diagonalShorts :: Board
diagonalShorts = array ((0,0),(3,3))
        [
        ((0, 0), Empty),
        ((0, 1), Empty),
        ((0, 2), Empty),
        ((0, 3), Cell (Piece White Circular Short Solid)),
        ((1, 0), Empty),
        ((1, 1), Empty),
        ((1, 2), Cell (Piece Black Circular Short Solid)),
        ((1, 3), Empty),
        ((2, 0), Empty),
        ((2, 1), Cell (Piece White Circular Short Hollow)),
        ((2, 2), Empty),
        ((2, 3), Empty),
        ((3, 0), Cell (Piece White Square Short Hollow)),
        ((3, 1), Empty),
        ((3, 2), Empty),
        ((3, 3), Empty)
        ]

threeShorts :: Board
threeShorts = array ((0,0),(3,3))
        [
        ((0, 0), Empty),
        ((0, 1), Empty),
        ((0, 2), Empty),
        ((0, 3), Cell (Piece White Circular Short Solid)),
        ((1, 0), Empty),
        ((1, 1), Empty),
        ((1, 2), Cell (Piece Black Circular Short Solid)),
        ((1, 3), Empty),
        ((2, 0), Empty),
        ((2, 1), Cell (Piece White Circular Short Hollow)),
        ((2, 2), Empty),
        ((2, 3), Empty),
        ((3, 0), Empty),
        ((3, 1), Empty),
        ((3, 2), Empty),
        ((3, 3), Empty)
        ]

printBoardSpec :: Spec
printBoardSpec =
        describe "printBoard" $ do
          it "empty board should be just an empty grid" $
            printBoard initialBoard `shouldBe` empty
          it "pieces should be printed correctly" $
            printBoard diagonalShorts `shouldBe` diagonals
        where empty = "+----+----+----+----+\n" ++
                      "|    |    |    |    |\n" ++
                      "+----+----+----+----+\n" ++
                      "|    |    |    |    |\n" ++
                      "+----+----+----+----+\n" ++
                      "|    |    |    |    |\n" ++
                      "+----+----+----+----+\n" ++
                      "|    |    |    |    |\n" ++
                      "+----+----+----+----+\n"
              diagonals = "+----+----+----+----+\n" ++
                          "|    |    |    |cshT|\n" ++
                          "+----+----+----+----+\n" ++
                          "|    |    |CshT|    |\n" ++
                          "+----+----+----+----+\n" ++
                          "|    |csht|    |    |\n" ++
                          "+----+----+----+----+\n" ++
                          "|cSht|    |    |    |\n" ++
                          "+----+----+----+----+\n"
