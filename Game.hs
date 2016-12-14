module Game where
    import qualified Tiles
    import qualified Players
    data Board = Board [[Tiles.Tile]]

    data Game = Game [Players.Player] Board Tiles.Tile