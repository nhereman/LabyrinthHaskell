module Game where
    import qualified Tiles
    import qualified Players
    
    data Board = Board [[Tiles.Tile]]

    data Game = Game {players :: [Players.Player]
                      , board :: Board
                      , xtile :: Tiles.Tile}

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
                            isLeftSide (x,y) = x == 0
                            isRightSide (x,y) = x == 6
                            isTopSide (x,y) = y == 0
                            isBotSide (x,y) = y == 6
                            leftSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.West
                            rightSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.East
                            topSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.North
                            botSideTile = Tiles.Tile Tiles.Tshaped 0 Tiles.South

    getStartTile :: Players.Position -> Tiles.Tile
    getStartTile pos
                | pos == (0,0) = Tiles.Tile Tiles.Corner 0 Tiles.East
                | pos == (0,6) = Tiles.Tile Tiles.Corner 0 Tiles.North
                | pos == (6,0) = Tiles.Tile Tiles.Corner 0 Tiles.South
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

    fillBoardIgnoreFixed :: Board -> Players.Position -> [Tiles.Tile] -> Board
    fillBoardIgnoreFixed board pos [] = board
    fillBoardIgnoreFixed board pos (t:ts) = fillBoardIgnoreFixed (replaceTile board pos t) (nextPos pos) ts
                where
                    nextPos (x,y) = if x == 6 then (0,y+1) else (x+1,y)

    generateBoard :: [Tiles.Tile] -> Board
    generateBoard tiles = fillBoard (generateFixedTiles generateEmptyBoard) (0,0) tiles

    loadBoard :: [Tiles.Tile] -> Board
    loadBoard tiles = fillBoardIgnoreFixed generateEmptyBoard (0,0) tiles


    -- Game generation

    generateGame :: [Players.Player] -> [Players.Card] -> [Tiles.Tile] -> [Tiles.Treasure] -> Game
    generateGame players cards (t:ts) (tr:trs) = Game (Players.distributeCards players cards)
                                                            (putTreasureOnBoard (generateBoard ts) 0 0 trs)
                                                            (Tiles.putTreasureOnTile t tr)

    putTreasureOnBoard :: Board -> Int -> Int  -> [Tiles.Treasure] -> Board
    putTreasureOnBoard board _ _ [] = board
    putTreasureOnBoard board col row (t:ts)
                        | col == 7 && row == 7 = board
                        | col == 7 = putTreasureOnBoard board 0 (row+1) (t:ts)
                        | otherwise = replaceTile (putTreasureOnBoard board (col+1) row ts) (col,row)
                                                    (Tiles.putTreasureOnTile (getBoardTile board col row) t)



    -- Maze shift
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

    -- Pawn movement

    reachablePosPlayer :: Board -> Players.Player -> [Players.Position]
    reachablePosPlayer board (Players.Player _ _ pos _) = reachablePos board pos []


    reachablePos :: Board -> Players.Position -> [Players.Position] -> [Players.Position]
    reachablePos board (x,y) visited = visitBot
                    where
                        isLeftReachable = (x>0) && (Tiles.isConnectedByLeft (getBoardTile board x y) (getBoardTile board (x-1) y))
                                        && not ( (x-1,y) `elem` visited )
                        isRightReachable = (x<6) && (Tiles.isConnectedByRight (getBoardTile board x y) (getBoardTile board (x+1) y))
                                         && not ( (x+1,y) `elem` visited)
                        isTopReachable = (y>0) && (Tiles.isConnectedByTop (getBoardTile board x y) (getBoardTile board x (y-1)))
                                        && not ( (x,y-1) `elem` visited)
                        isBotReachable = (y<6) && (Tiles.isConnectedByBot (getBoardTile board x y) (getBoardTile board x (y+1)))
                                          && not ( (x,y+1) `elem` visited)
                        visitLeft = if isLeftReachable then reachablePos board (x-1,y) ((x,y):visited) else (x,y):visited
                        visitTop = if isTopReachable then reachablePos board (x,y-1) visitLeft else visitLeft
                        visitRight = if isRightReachable then reachablePos board (x+1,y) visitTop else visitTop
                        visitBot = if isBotReachable then reachablePos board (x,y+1) visitRight else visitRight

    movePlayerTo :: Game -> Players.Position -> Game
    movePlayerTo (Game ((Players.Player col ctrl _ cards):ps) board xtile) pos = Game (newPlayer:ps) board xtile
                    where
                        newPlayer = Players.Player col ctrl pos cards

    -- Draw board

    drawBoard :: Game -> String
    drawBoard (Game players board _) = before ++ drawBoard' board players
            where
                before = "---0-------1-------2-------3-------4-------5-------6----\n"

    drawBoard' :: Board -> [Players.Player] -> String
    drawBoard' board players = drawBoard'' board 0 players

    drawBoard'' :: Board -> Int -> [Players.Player] -> String
    drawBoard'' board row players
            | row == 7 = "\n"
            | otherwise = drawRow board row 0 0 players ++ drawBoard'' board (row+1) players

    drawRow :: Board -> Int -> Int -> Int -> [Players.Player] -> String
    drawRow board row col tLine players
            | tLine == 5 = "--------------------------------------------------------\n"
            | col == 7 = "\n"++drawRow board row 0 (tLine+1) players
            | endRow && isRBStart = (Tiles.asciiTile (getBoardTile board col row) getRBStart pos) !! tLine ++ show row ++ drawRow board row (col+1) tLine players
            | endRow = (Tiles.asciiTile (getBoardTile board col row) "" pos) !! tLine ++ show row ++ drawRow board row (col+1) tLine players
            | isYGStart = (Tiles.asciiTile (getBoardTile board col row) getYGStart pos) !! tLine ++ "|" ++ drawRow board row (col+1) tLine players
            | otherwise = (Tiles.asciiTile (getBoardTile board col row) "" pos) !! tLine ++ "|" ++ drawRow board row (col+1) tLine players
                where
                    endRow = tLine == 2 && col == 6
                    isRBStart = col == 6 && ( (row == 0) || (row == 6) )
                    getRBStart = if (row == 0) then "R" else "B"
                    isYGStart = col == 0 && ( (row == 0) || (row == 6))
                    getYGStart = if (row == 0)  then "Y" else "G"
                    pos = (isPlayerAt players (col,row) Players.Yellow, isPlayerAt players (col,row) Players.Red,
                          isPlayerAt players (col,row) Players.Green, isPlayerAt players (col,row) Players.Blue)

    isPlayerAt :: [Players.Player] -> Players.Position -> Players.Color -> Bool
    isPlayerAt [] _ _ = False
    isPlayerAt ((Players.Player col _ pos _):ps) pos2 col2 = (col == col2 && pos == pos2) || isPlayerAt ps pos2 col2


    -- Turn management

    nextTurn :: Game -> Game
    nextTurn (Game (p:ps) board xtile) =Game (ps++[p]) board xtile


    -- Treasure gathering

    gatherTreasure :: Game -> Game
    gatherTreasure (Game ((Players.Player col ctrl (x,y) cards):ps) board tile)
                    | treasure `elem` cards = Game (newPlayer:ps) board tile
                    | otherwise = (Game ((Players.Player col ctrl (x,y) cards):ps) board tile) 
                    where
                        treasure = (Tiles.treasure (getBoardTile board x y))
                        newPlayer = Players.Player col ctrl (x,y) [c | c <- cards, not (c == treasure)]

    reachableTreasureNeeded :: Game -> [Tiles.Treasure]
    reachableTreasureNeeded (Game ((Players.Player col ctrl pos cards):ps) board xtile) = 
                            filter (\t -> t `elem` cards) $ toTreasure $ reachablePos board pos []
                    where
                        toTreasure [] = []
                        toTreasure ((x,y):ps) =( Tiles.treasure (getBoardTile board x y)):toTreasure ps

    reachableTreasureNeededPos :: Game -> [Players.Position]
    reachableTreasureNeededPos (Game ((Players.Player col ctrl pos cards):ps) board xtile) =
                            filter (\pos -> isNeeded pos) $ reachablePos board pos []
                    where
                        isNeeded pos = getTreasure pos `elem` cards
                        getTreasure (x,y) = Tiles.treasure (getBoardTile board x y)

    neededTreasurePos :: Game -> [Players.Position]
    neededTreasurePos game = []

    neededTreasurePos' :: Board -> [Players.Card] -> Int -> Int -> [Players.Position]
    neededTreasurePos' board cards col row
                    | row == 7 = []
                    | col == 7 = neededTreasurePos' board cards 0 (row+1)
                    | isTreasureNeeded = (col,row):neededTreasurePos' board cards (col+1) row
                    | otherwise = neededTreasurePos' board cards (col+1) row
                    where
                        isTreasureNeeded = (Tiles.treasure (getBoardTile board col row)) `elem` cards


    -- Win condition

    playerHasWin :: Game -> Bool
    playerHasWin (Game ((Players.Player col _ pos cards):ps) _ _) = length cards == 0 && pos == Players.colorPosition col

    -- AI

    playerIsAI :: Game -> Bool
    playerIsAI (Game ((Players.Player _ ctrl _ _):_) _ _) = ctrl == Players.AI










