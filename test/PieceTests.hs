module PieceTests where

import Test.Hspec
import Piece

pieceSpec :: IO ()
pieceSpec = hspec $
        describe "Piece"
          commonPropertySpec

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
