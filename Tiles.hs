module Tiles where

    type Treasure = Int
    data Direction = North | East | South | West deriving (Show)
    data Kind = Corner | Tshaped | Line deriving (Show)


    data Tile = Tile {
                kind :: Kind
                , treasure :: Treasure
                , dir :: Direction
                } deriving (Show)


    generateEmptyTiles :: [Tile]
    generateEmptyTiles = (take 16 (repeat (Tile Corner 0 North)))
                         ++ (take 6 (repeat (Tile Tshaped 0 North)))
                         ++ (take 12 (repeat (Tile Line 0 North)))





    