import Test.Hspec
import Board
import Data.Array
import Data.List

main :: IO ()
main = boardSpec

boardSpec :: IO ()
boardSpec = hspec $
        describe "Board"
          initialBoardSpec

initialBoardSpec :: Spec
initialBoardSpec =
        describe "initialBoard" $ do
         it "should have only empty squares" $
           nub (elems initialBoard) `shouldBe` [Empty]
         it "should have 4x4 squares" $
           length (elems initialBoard) `shouldBe` (4 * 4)
