module Tiles where

    type Treasure = Int
    data Direction = North | East | South | West deriving (Eq)
    data Kind = Corner | Tshaped | Line deriving (Eq)


    data Tile = Tile {
                kind :: Kind
                , treasure :: Treasure
                , dir :: Direction
                }


    -- Show Instance

    instance Show Direction where
        show dir
            | dir == North = "north"
            | dir == East = "east"
            | dir == South = "south"
            | otherwise = "west" 

    instance Show Kind where
        show kind
            | kind == Corner = "corner"
            | kind == Tshaped = "tshape"
            | otherwise = "line"

    instance Show Tile where
        show (Tile kind treasure dir) = show kind ++ " " ++ show treasure ++ " " ++ show dir

    showXTile :: Tile -> String
    showXTile  (Tile kind treasure _) = show kind ++ " " ++ show treasure


    -- Tiles generation

    generateEmptyTiles :: [Tile]
    generateEmptyTiles = (take 16 (repeat (Tile Corner 21 North)))
                         ++ (take 6 (repeat (Tile Tshaped 22 North)))
                         ++ (take 12 (repeat (Tile Line 23 North)))


    -- Tile connection

    isConnectedByTop :: Tile -> Tile -> Bool
    isConnectedByTop tile tileTop = (hasTopEntry tile) && (hasBotEntry tileTop)

    isConnectedByBot :: Tile -> Tile -> Bool
    isConnectedByBot tile tileBot = (hasBotEntry tile) && (hasTopEntry tileBot)

    isConnectedByLeft :: Tile -> Tile -> Bool
    isConnectedByLeft tile tileLeft = (hasLeftEntry tile) && (hasRightEntry tileLeft)

    isConnectedByRight :: Tile -> Tile -> Bool
    isConnectedByRight tile tileRight = (hasRightEntry tile) && (hasLeftEntry tileRight)


    hasBotEntry :: Tile -> Bool
    hasBotEntry (Tile kind _ dir) = (kind == Corner && (dir == East || dir == South))
                                    || (kind == Line && (dir == North || dir == South))
                                    || (kind == Tshaped && (dir /= South))

    hasTopEntry :: Tile -> Bool
    hasTopEntry (Tile kind _ dir) = (kind == Corner && (dir == North || dir == West))
                                    || (kind == Line && (dir == North || dir == South))
                                    || (kind == Tshaped && (dir /= North))

    hasLeftEntry :: Tile -> Bool
    hasLeftEntry (Tile kind _ dir) = (kind == Corner && (dir == West || dir == South))
                                    || (kind == Line && (dir == East || dir == West))
                                    || (kind == Tshaped && (dir /= West))

    hasRightEntry :: Tile -> Bool
    hasRightEntry (Tile kind _ dir) = (kind == Corner && (dir == North || dir == East))
                                        || (kind == Line && (dir == East || dir == West))
                                        || (kind == Tshaped && (dir /= East))




    -- Draw TILES

    asciiTile :: Tile -> String -> (Bool,Bool,Bool,Bool) -> [String]
    asciiTile (Tile kind treasure dir) start pos
                | kind == Tshaped = asciiTshaped treasure dir pos
                | kind == Corner = asciiCorner treasure dir start pos
                | otherwise = asciiLine treasure dir pos

    asciiTshaped :: Treasure -> Direction -> (Bool,Bool,Bool,Bool) -> [String]
    asciiTshaped treasure dir (yellow,red,green,blue)
                | dir == North = [y++"     "++r, "═══════", " "++t++"    ", "══╗ ╔══", g++" ║ ║ "++b]
                | dir == South = [y++" ║ ║ "++r, "══╝ ╚══", " "++t++"    ", "═══════", g++"     "++b]
                | dir == West =  [y++" ║ ║ "++r, "  ║ ╚══", "  ║"++t++"  ", "  ║ ╔══", g++" ║ ║ "++b]
                | otherwise =    [y++" ║ ║ "++r, "══╝ ║  ", " "++t++" ║  ", "══╗ ║  ", g++" ║ ║ "++b]
                where t = asciiTreasure treasure
                      y = if yellow then "Y" else " "
                      r = if red then "R" else " "
                      g = if green then "G" else " "
                      b = if blue then "B" else " "

    asciiCorner :: Treasure -> Direction -> String -> (Bool,Bool,Bool,Bool) -> [String]
    asciiCorner treasure dir start (yellow,red,green,blue)
                | dir == North = [y++" ║ ║ "++r, "  ║ ╚══", "  ║"++t++"  ", "  ╚════", g++"     "++b]
                | dir == South = [y++"     "++r, "════╗  ", "  "++t++"║  ", "══╗ ║  ", g++" ║ ║ "++b]
                | dir == West =  [y++" ║ ║ "++r, "══╝ ║  ", "  "++t++"║  ", "════╝  ", g++"     "++b]
                | otherwise =    [y++"     "++r, "  ╔════", "  ║"++t++"  ", "  ║ ╔══", g++" ║ ║ "++b]
                where t = if start == "" then asciiTreasure treasure else " "++start
                      y = if yellow then "Y" else " "
                      r = if red then "R" else " "
                      g = if green then "G" else " "
                      b = if blue then "B" else " "

    asciiLine :: Treasure -> Direction -> (Bool,Bool,Bool,Bool) -> [String]
    asciiLine treasure dir (yellow,red,green,blue)
                | dir == North || dir == South = [y++" ║ ║  ", "  ║"++[t!!0]++"║  ", "  ║"++[t!!1]++"║  ", "  ║ ║  ", g++" ║ ║ "++b]
                | otherwise = [y++"      ", "═══════", "  "++t++"   ", "═══════", g++"     "++b]
                where t = asciiTreasure treasure
                      y = if yellow then "Y" else " "
                      r = if red then "R" else " "
                      g = if green then "G" else " "
                      b = if blue then "B" else " "


    asciiTreasure :: Treasure -> String
    asciiTreasure treasure
                | treasure == 0 = "  "
                | treasure < 10 = " " ++ show treasure
                | otherwise = show treasure 




