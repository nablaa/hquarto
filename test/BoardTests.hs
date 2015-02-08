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
