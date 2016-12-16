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
    generateEmptyTiles = (take 16 (repeat (Tile Corner 0 North)))
                         ++ (take 6 (repeat (Tile Tshaped 0 North)))
                         ++ (take 12 (repeat (Tile Line 0 North)))
