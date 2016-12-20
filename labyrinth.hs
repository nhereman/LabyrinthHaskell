import qualified Tiles
import qualified Players
import qualified Game

main :: IO ()
main = stateOfTheGame $ Game.generateGame 2 2 Tiles.generateEmptyTiles


stateOfTheGame :: Game.Game -> IO ()
stateOfTheGame (Game.Game players board xtile) = do 
                                                    putStr $ Game.drawBoard (Game.Game players board xtile)
                                                    putStr "Free tile :\n"
                                                    putStr $ unlines $ Tiles.asciiTile xtile "" (False,False,False,False)
                                                    whichPlayer players
                                                    playerCards players

whichPlayer :: [Players.Player] -> IO ()
whichPlayer ((Players.Player col _ _ _):_) = putStr $ "Player : " ++ show col ++ "\n"

playerCards :: [Players.Player] -> IO ()
playerCards ((Players.Player _ _ _ cards):_) = do
                                                let c = Players.showCards cards
                                                let str = if cards /= [] then c else "You have all your treasures, go to you starting tile in orderto win"
                                                putStr $ "Your cards : "++str++"\n"







