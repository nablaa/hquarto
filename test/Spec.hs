import BoardTests
import PieceTests
import GameTests

main :: IO ()
main = do pieceSpec
          boardSpec
          gameSpec
