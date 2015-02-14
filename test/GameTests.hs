module GameTests where

import Test.Hspec
import Game
import Piece

gameSpec :: IO ()
gameSpec = hspec $
        describe "GameState"
          makeMoveSpec

makeMoveSpec :: Spec
makeMoveSpec =
        describe "makeMove" $ do
          it "making move with invalid coordinates should fail" $ do
            makeMove initialGame (-1, 0) (Piece Black Square Tall Hollow) `shouldBe` Nothing
            makeMove initialGame (3, 4) (Piece Black Square Tall Hollow) `shouldBe` Nothing
          it "making move with next piece as current piece should fail" $
            makeMove initialGame (0, 0) firstPiece `shouldBe` Nothing
          it "making move with valid coordinates and valid next piece should succeed" $
            makeMove initialGame (0, 0) (Piece Black Square Tall Hollow) /= Nothing `shouldBe` True
        where firstPiece = Piece White Square Tall Hollow
              initialGame = newGame firstPiece
