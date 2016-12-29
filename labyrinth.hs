import qualified Tiles
import qualified Players
import qualified Game
import qualified System.Process
import qualified Utils
import qualified AI
import System.Random

main :: IO ()
main = createGame


createGame :: IO ()
createGame = do
                gen <- getStdGen
                let (treasures,gen2) = Utils.permutation gen [1..24]
                let (tiles,gen3) = Utils.permutation gen2 Tiles.generateEmptyTiles
                let (treasTile,gen4) = Utils.permutation gen3 $ Tiles.putTreasureOnTiles tiles treasures
                let (cards,gen5) = Utils.permutation gen4 [1..24]
                turn $ Game.generateGame 1 0 cards treasTile


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
turn game = if Game.playerIsAI game then playAI game else turnHuman game

turnHuman :: Game.Game -> IO ()
turnHuman game = do
                System.Process.callCommand "clear"
                stateOfTheGame game
                choice <- askSaveOrPlay
                if choice == "0" then playHuman game else save game

playHuman :: Game.Game -> IO ()
playHuman game = do
                System.Process.callCommand "clear"
                stateOfTheGame game
                shifted <- mazeShiftingChoice game
                let collected = Game.gatherTreasures shifted
                System.Process.callCommand "clear"
                stateOfTheGame collected
                pos <- moveChoice collected
                let moved = Game.movePlayerTo collected pos
                let hasWin = Game.playerHasWin moved
                endOfTurn moved hasWin

playAI :: Game.Game -> IO ()
playAI game = do
                let collected = AI.bestMove game
                let moved = AI.movePawn collected
                let hasWin = Game.playerHasWin moved
                endOfTurn moved hasWin
                


endOfTurn :: Game.Game -> Bool -> IO ()
endOfTurn game hasWin = if hasWin then announceWinner game else turn $ Game.nextTurn game

announceWinner :: Game.Game -> IO ()
announceWinner (Game.Game ((Players.Player col _ _ _):_) _ _) = putStr $ show col ++ " has won the game ! \n"

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
                    choice2 <- if isChoiceSPGood choice then toIO choice else askSaveOrPlay
                    return choice2

toIO :: a -> IO a
toIO dat = return dat -- FIND A BETTER WAY !!!!!!!!!!!!!!!!

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
                        choice2 <- if choice `elem` ["0","1","2","3"] then toIO choice else insertSideChoice
                        return choice2

rowOrColChoice :: Bool -> IO String
rowOrColChoice isCol = do
                        putStr $"In which "++rowOrCol++" do you want to insert the tile ? \n"
                        putStr "1, 3 or 5 ?\n"
                        choice <- getLine
                        choice2 <- if choice `elem` ["1", "3", "5"] then toIO choice else rowOrColChoice isCol
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
                    choice2 <- if choice `elem` ["0","1","2","3"] then toIO choice else positionChoice
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


-- Pawn movement

moveChoice :: Game.Game -> IO (Int,Int)
moveChoice  (Game.Game (p:ps) board tile) = do
                    putStr "Where do you want your pawn to move (reachable tile only !) ?\n"
                    let reachable  = (Game.reachablePosPlayer board p)
                    col <- colChoice
                    row <- rowChoice
                    result <- if (col,row) `elem` reachable
                                then toIO (col,row)
                                else moveChoice (Game.Game (p:ps) board tile)
                    return result

colChoice :: IO Int
colChoice = do
                putStr "Which column you want your pawn to go (0 to 6) ?\n"
                colStr <- getLine
                let col = read colStr :: Int
                result <- if col >= 0 && col < 7 then toIO col else colChoice
                return result

rowChoice :: IO Int
rowChoice = do
                putStr "Which row you want your pawn to go (0 to 6) ?\n"
                rowStr <- getLine
                let row = read rowStr :: Int
                result <- if row >= 0 && row < 7 then toIO row else rowChoice
                return result





