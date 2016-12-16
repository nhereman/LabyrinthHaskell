import qualified Tiles
import qualified Players
import qualified Game

main :: IO ()
main = putStr $ show $ Game.reachablePos (Game.board (Game.generateGame 1 1 Tiles.generateEmptyTiles)) [(0,0)]