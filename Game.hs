module Game where
    import qualified Tiles
    import qualified Players
    
    data Board = Board [[Tiles.Tile]] deriving (Show)

    data Game = Game [Players.Player] Board Tiles.Tile deriving (Show)



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