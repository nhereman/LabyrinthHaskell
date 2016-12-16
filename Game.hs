module Game where
    import qualified Tiles
    import qualified Players
    
    data Board = Board [[Tiles.Tile]]

    data Game = Game [Players.Player] Board Tiles.Tile

    -- Show Instance

    instance Show Board where
        show (Board board) = showBoard board 0 0


    showBoard :: [[Tiles.Tile]] -> Int -> Int -> String
    showBoard board x y
            | x == 7 = showBoard board 0 (y+1)
            | (x,y) == (6,6) = show (board !! x !! y)
            | otherwise = show (board !! x !! y) ++ " " ++ showBoard board (x+1) y

    instance Show Game where
        show (Game players board xtile) = Players.showPlayers players ++ " " ++ Tiles.showXTile xtile ++ " " ++ show board


    -- Save game

    saveGame :: Game -> FilePath -> IO ()
    saveGame game path = writeFile path $ show game



    -- Fixed Board generation

    isFixedPosition :: Players.Position -> Bool
    isFixedPosition (x,y) = (x `mod` 2 == 0) && (y `mod`2 == 0)

    allFixedPosition :: [Players.Position]
    allFixedPosition = [(x,y) | x <- [0..6], y <- [0..6], isFixedPosition (x,y)]

    isStartingPosition :: Players.Position -> Bool
    isStartingPosition pos = isFixedPosition pos && pos `elem` [(0,0),(6,0),(0,6),(6,6)]

    generateEmptyBoard :: Board
    generateEmptyBoard = Board $ take 7 $ repeat $ take 7 $ repeat $ Tiles.Tile Tiles.Line 0 Tiles.North

    generateFixedTiles :: Board -> Board
    generateFixedTiles board = generateFixedTiles' board allFixedPosition

    generateFixedTiles' :: Board -> [Players.Position] -> Board
    generateFixedTiles' board [] = board
    generateFixedTiles' board (p:ps)
                        | isStartingPosition p = generateFixedTiles' (replaceTile board p (getStartTile p)) ps
                        | isLeftSide p = generateFixedTiles' (replaceTile board p leftSideTile) ps
                        | isRightSide p = generateFixedTiles' (replaceTile board p rightSideTile) ps
                        | isTopSide p = generateFixedTiles' (replaceTile board p topSideTile) ps
                        | isBotSide p = generateFixedTiles' (replaceTile board p botSideTile) ps
                        | otherwise = generateFixedTiles' (replaceTile board p (getCenterTile p)) ps
                        where
                            isLeftSide (x,y) = y == 0
                            isRightSide (x,y) = y == 6
                            isTopSide (x,y) = x == 0
                            isBotSide (x,y) = x == 6
                            leftSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.West
                            rightSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.East
                            topSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.North
                            botSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.South

    getStartTile :: Players.Position -> Tiles.Tile
    getStartTile pos
                | pos == (0,0) = Tiles.Tile Tiles.Corner 0 Tiles.East
                | pos == (0,6) = Tiles.Tile Tiles.Corner 0 Tiles.South
                | pos == (6,0) = Tiles.Tile Tiles.Corner 0 Tiles.North
                | otherwise = Tiles.Tile Tiles.Corner 0 Tiles.West

    getCenterTile :: Players.Position -> Tiles.Tile
    getCenterTile pos
                | pos == (2,2) = Tiles.Tile Tiles.Tshaped 0 Tiles.West
                | pos == (4,2) = Tiles.Tile Tiles.Tshaped 0 Tiles.North
                | pos == (2,4) = Tiles.Tile Tiles.Tshaped 0 Tiles.South
                | otherwise = Tiles.Tile Tiles.Tshaped 0 Tiles.East


    replaceTile :: Board -> Players.Position -> Tiles.Tile -> Board
    replaceTile (Board board) (x,y) tile = Board (take x board ++ [take y (board!!x) ++ tile : drop (y+1) (board!!x)] ++ drop (x+1) board)

    getBoardTile :: Board -> Int -> Int -> Tiles.Tile
    getBoardTile (Board board) col row = board !! col !! row

    -- Board generation

    fillBoard :: Board -> Players.Position -> [Tiles.Tile] -> Board
    fillBoard board pos [] = board
    fillBoard board pos (t:ts)
                | isFixedPosition pos = fillBoard board (nextPos pos) (t:ts)
                | otherwise = fillBoard (replaceTile board pos t) (nextPos pos) ts
                where
                    nextPos (x,y) = if x == 6 then (0,y+1) else (x+1,y)

    generateBoard :: [Tiles.Tile] -> Board
    generateBoard tiles = fillBoard (generateFixedTiles generateEmptyBoard) (0,0) tiles


    -- Game generation

    generateGame :: Int -> Int -> [Tiles.Tile] -> Game
    generateGame nbPlayer nbAI (t:ts) = Game (Players.generatePlayers nbPlayer nbAI) (generateBoard ts) t



    -- Gameplay
    insertBottom :: Game -> Int -> Game
    insertBottom (Game players (Board board) tile) col = Game newPlayers newBoard newTile
                where
                    newPlayers = movePlayersUp players col
                    newBoard = Board $ take col board ++ [drop 1 (board !! col) ++ [tile]] ++ drop (col+1) board
                    newTile = (board !! col) !! 0
    
    movePlayersUp :: [Players.Player] -> Int -> [Players.Player]
    movePlayersUp [] _ = []
    movePlayersUp ((Players.Player color ctrl (x,y) card):ps) col = [if x == col then Players.Player color ctrl (x,newY) card
                                                                    else Players.Player color ctrl (x,y) card]
                                                                    ++ movePlayersUp ps col
                where
                    newY = if (y-1) < 0 then 6 else (y-1)


    insertTop :: Game -> Int -> Game
    insertTop (Game players (Board board) tile) col = Game newPlayers newBoard newTile
                where
                    newPlayers = movePlayersUp players col
                    newBoard = Board $ take col board ++ [tile:take 6 (board !! col)] ++ drop (col+1) board
                    newTile = (board !! col) !! 6
    
    movePlayersDown :: [Players.Player] -> Int -> [Players.Player]
    movePlayersDown [] _ = []
    movePlayersDown ((Players.Player color ctrl (x,y) card):ps) col = [if x == col then Players.Player color ctrl (x,newY) card
                                                                    else Players.Player color ctrl (x,y) card]
                                                                    ++ movePlayersUp ps col
                where
                    newY = if (y+1) > 6 then 0 else (y+1)

    insertLeft :: Game -> Int -> Game
    insertLeft (Game players (Board board) tile) row = Game newPlayers newBoard newTile
                where
                    newPlayers = movePlayersRight players row
                    newBoard = moveRowRight (Board board) tile row 6
                    newTile = board !! 6 !! row

    movePlayersRight :: [Players.Player] -> Int -> [Players.Player]
    movePlayersRight [] _ = []
    movePlayersRight ((Players.Player color ctrl (x,y) card):ps) row = [if y == row then Players.Player color ctrl (newX,y) card
                                                                        else Players.Player color ctrl (x,y) card]
                                                                        ++ movePlayersRight ps row
                    where
                        newX = if (x+1) > 6 then 0 else (x+1)

    moveRowRight :: Board -> Tiles.Tile -> Int -> Int -> Board
    moveRowRight board tile row col
                | col == 0 = replaceTile board (col,row) tile
                | otherwise = replaceTile next (col,row) leftTile
                where
                    next = moveRowRight board tile row (col-1)
                    leftTile = getBoardTile board (col-1) row

    insertRight :: Game -> Int -> Game
    insertRight (Game players (Board board) tile) row = Game newPlayers newBoard newTile
                where
                    newPlayers = movePlayersLeft players row
                    newBoard = moveRowLeft (Board board) tile row 0
                    newTile = board !! 0 !! row

    movePlayersLeft :: [Players.Player] -> Int -> [Players.Player]
    movePlayersLeft [] _ = []
    movePlayersLeft ((Players.Player color ctrl (x,y) card):ps) row = [if y == row then Players.Player color ctrl (newX,y) card
                                                                        else Players.Player color ctrl (x,y) card]
                                                                        ++ movePlayersLeft ps row
                    where
                        newX = if (x-1) < 0 then 6 else (x-1)

    moveRowLeft :: Board -> Tiles.Tile -> Int -> Int -> Board
    moveRowLeft board tile row col
                | col == 6 = replaceTile board (col,row) tile
                | otherwise = replaceTile next (col,row) rightTile
                where
                    next = moveRowLeft board tile row (col+1)
                    rightTile = getBoardTile board (col+1) row