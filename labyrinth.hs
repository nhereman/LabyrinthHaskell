import qualified Tiles
import qualified Players
import qualified Game
import qualified System.Process

main :: IO ()
main = turn $ Game.generateGame 1 1 [1..24] $ Tiles.putTreasureOnTiles Tiles.generateEmptyTiles [1..24]


stateOfTheGame :: Game.Game -> IO ()
stateOfTheGame (Game.Game players board xtile) = do 
                                                    putStr $ Game.drawBoard (Game.Game players board xtile)
                                                    putStr "Free tile :\n"
                                                    putStr $ unlines $ Tiles.asciiTile xtile "" (False,False,False,False)
                                                    whichPlayer players
                                                    playerCards players
                                                    otherPlayerCards $ tail players

whichPlayer :: [Players.Player] -> IO ()
whichPlayer ((Players.Player col _ _ _):_) = putStr $ "Player : " ++ show col ++ "\n"

playerCards :: [Players.Player] -> IO ()
playerCards ((Players.Player _ _ _ cards):_) = do
                                                let c = Players.showCards cards
                                                let str = if cards /= [] then c else "You have all your treasures, go to you starting tile in order to win"
                                                putStr $ "Your cards : "++str++"\n"

otherPlayerCards :: [Players.Player] -> IO ()
otherPlayerCards [] = putStr "\n"
otherPlayerCards ((Players.Player col _ _ cards):ps) = do
                                                            putStr $ show col ++ ":" ++ show (length cards) ++ " "
                                                            otherPlayerCards ps



-- Turn

turn :: Game.Game -> IO ()
turn game = do
                System.Process.callCommand "clear"
                stateOfTheGame game
                choice <- askSaveOrPlay
                if choice == "0" then play game else save game

play :: Game.Game -> IO ()
play game = do
                System.Process.callCommand "clear"
                stateOfTheGame game
                shifted <- mazeShiftingChoice game
                let collected = Game.gatherTreasures shifted
                turn $ Game.nextTurn collected


save :: Game.Game -> IO ()
save game = do
                putStr "Filename : \n"
                filepath <- getLine
                Game.saveGame game filepath


-- Choice save or play
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



-- Maze Shifting

mazeShiftingChoice :: Game.Game -> IO Game.Game
mazeShiftingChoice game = do
                            side <- insertSideChoice
                            System.Process.callCommand "clear"
                            stateOfTheGame game
                            rowOrCol <- if side == "0" || side == "1" then rowOrColChoice True else rowOrColChoice False
                            let rcInt = read rowOrCol :: Int
                            System.Process.callCommand "clear"
                            stateOfTheGame game
                            showXtileChoice game
                            pos <- positionChoice
                            return $ choiceToAction game side pos rcInt

insertSideChoice :: IO String
insertSideChoice = do
                        putStr "From which side do you want to insert the tile ?\n"
                        putStr "0.Top\t1.Bottom\t2.Left\t3.Right\n"
                        choice <- getLine
                        choice2 <- if choice `elem` ["0","1","2","3"] then stringToIO choice else insertSideChoice
                        return choice2

rowOrColChoice :: Bool -> IO String
rowOrColChoice isCol = do
                        putStr $"In which "++rowOrCol++" do you want to insert the tile ? \n"
                        putStr "1, 3 or 5 ?\n"
                        choice <- getLine
                        choice2 <- if choice `elem` ["1", "3", "5"] then stringToIO choice else rowOrColChoice isCol
                        return choice2
                        where
                            rowOrCol = if isCol then "column" else "row"

showXtileChoice :: Game.Game -> IO ()
showXtileChoice (Game.Game _ _ (Tiles.Tile kind treasures _)) = do
                                                                    putStr $ "0.\n" ++ unlines (tile Tiles.North) ++ "\n"
                                                                    putStr $ "1.\n" ++ unlines (tile Tiles.East) ++ "\n"
                                                                    putStr $ "2.\n" ++ unlines (tile Tiles.South) ++ "\n"
                                                                    putStr $ "3.\n" ++ unlines (tile Tiles.West) ++ "\n"
                                                                where
                                                                    tile d = Tiles.asciiTile (Tiles.Tile kind treasures d) "" (False,False,False,False)

positionChoice :: IO String
positionChoice = do
                    putStr "Which position for the tile (0,1,2 or 3) ?\n"
                    choice <- getLine
                    choice2 <- if choice `elem` ["0","1","2","3"] then stringToIO choice else positionChoice
                    return choice2

choiceToAction :: Game.Game -> String -> String -> Int -> Game.Game
choiceToAction (Game.Game players board (Tiles.Tile k t _)) side pos rowcol
                | side == "0" = Game.insertTop newGame rowcol
                | side == "1" = Game.insertBottom newGame rowcol
                | side == "2" = Game.insertLeft newGame rowcol
                | otherwise = Game.insertRight newGame rowcol
                where
                    newGame = (Game.Game players board newTile)
                    newTile
                            | pos == "0" = Tiles.Tile k t Tiles.North
                            | pos == "1" = Tiles.Tile k t Tiles.East
                            | pos == "2" = Tiles.Tile k t Tiles.South
                            | otherwise = Tiles.Tile k t Tiles.West







