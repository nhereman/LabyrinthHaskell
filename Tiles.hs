module Tiles where
    type Treasure = Int
    data Direction = North | East | South | West
    data Kind = Corner | Tshape | Line


    data Tile = Tile {
                kind :: Kind
                , treasure :: Treasure
                , dir :: Direction
                }