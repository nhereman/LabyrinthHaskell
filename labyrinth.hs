import qualified Tiles
import qualified Players
import qualified Game

main :: IO ()
main = Game.saveGame (Game.generateGame 1 1 Tiles.generateEmptyTiles) "test.laby"