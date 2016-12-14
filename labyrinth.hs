import qualified Tiles
import qualified Players
import qualified Game

main :: IO ()
main = putStr $ show $ Players.generatePlayers 4 2