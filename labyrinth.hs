import qualified Tiles
import qualified Players
import qualified Game
import qualified System.Process

main :: IO ()
main = turn $ Game.generateGame 4 4 Tiles.generateEmptyTiles


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
                                                let str = if cards /= [] then c else "You have all your treasures, go to you starting tile in order to win"
                                                putStr $ "Your cards : "++str++"\n"




-- Turn

turn :: Game.Game -> IO ()
turn game = do
                System.Process.callCommand "clear"
                stateOfTheGame game
                choice <- askSaveOrPlay
                if choice == "0" then play game else save game

play :: Game.Game -> IO ()
play game = do
                turn $ Game.nextTurn game


save :: Game.Game -> IO ()
save game = do
                putStr "Filename : \n"
                filepath <- getLine
                Game.saveGame game filepath


askSaveOrPlay :: IO String
askSaveOrPlay = do
                    putStr "\n0. Continue to play\n1. Save the game and quit\nWhat do you want to do 0 or 1?\n"
                    choice <- getLine
                    choice2 <- if isChoiceSPGood choice then stringToIO choice else askSaveOrPlay
                    return choice2

stringToIO :: String -> IO String
stringToIO string = return string -- FIND A BETTER WAY !!!!!!!!!!!!!!!!

isChoiceSPGood :: String -> Bool
isChoiceSPGood choice = choice == "0" || choice == "1"









