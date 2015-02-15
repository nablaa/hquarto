module PieceTests where

import Test.Hspec
import Piece

pieceSpec :: IO ()
pieceSpec = hspec $
        describe "Piece" $ do
          commonPropertySpec
          printPieceSpec
          readPieceSpec

commonPropertySpec :: Spec
commonPropertySpec =
        describe "haveCommonProperty" $ do
          it "single piece has common property always" $
            haveCommonProperty [Piece White Square Tall Hollow] `shouldBe` True
          it "two pieces with different properties" $
            haveCommonProperty
                [Piece White Square Tall Hollow,
                 Piece Black Circular Short Solid] `shouldBe` False
          it "three tall pieces" $
            haveCommonProperty
                [Piece Black Square Tall Hollow,
                 Piece Black Circular Tall Solid,
                 Piece White Square Tall Hollow] `shouldBe` True
          it "four hollow pieces" $
            haveCommonProperty
                [Piece White Square Tall Hollow,
                 Piece Black Circular Short Hollow,
                 Piece White Square Tall Hollow,
                 Piece Black Square Tall Hollow] `shouldBe` True

printPieceSpec :: Spec
printPieceSpec =
        describe "printPiece" $
          it "should print lowercase or uppercase letters depending on the properties" $
            printPiece (Piece White Circular Tall Solid) `shouldBe` "csHT"

readPieceSpec :: Spec
readPieceSpec =
        describe "readPiece" $ do
          it "reading should fail with strings with wrong number of letters" $ do
            readPiece "" `shouldBe` Nothing
            readPiece "C" `shouldBe` Nothing
            readPiece "CS" `shouldBe` Nothing
            readPiece "CSH" `shouldBe` Nothing
            readPiece "CSHTT" `shouldBe` Nothing
          it "reading should succeed with correct input" $ do
            readPiece "csHT" `shouldBe` Just (Piece White Circular Tall Solid)
            readPiece "CsHT" `shouldBe` Just (Piece Black Circular Tall Solid)
            readPiece "CSht" `shouldBe` Just (Piece Black Square Short Hollow)
