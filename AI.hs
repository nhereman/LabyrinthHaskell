module AI where

    import qualified Tiles
    import qualified Players
    import qualified Game
    import Debug.Trace



    playTurn :: Game.Game -> Game.Game
    playTurn game
            | nbCards game == 0 = playToStart game
            | otherwise = playToTreasure game
            where
                nbCards (Game.Game ((Players.Player _ _ _ cards):_) _ _) = length cards


    playToStart :: Game.Game -> Game.Game
    playToStart game
                | snd shifted = moveToOnePos (fst shifted) [start game]
                | snd shiftedNoStart = moveToOnePos (fst shiftedNoStart) nearPos
                | otherwise = Game.movePlayerTo (fst shifted) (head (reachable game))
                where
                    start (Game.Game ((Players.Player col _ _ _):ps) _ _ ) = Players.colorPosition col
                    shifted = putTile game [start game]
                    shiftedNoStart = putTile game nearPos
                    nearPos = (nearPositionList [start game] 1) ++ (nearPositionList [start game] 2) ++ (nearPositionList [start game] 3)
                    reachable (Game.Game (p:ps) board _) = Game.reachablePosPlayer board p

    playToTreasure :: Game.Game -> Game.Game
    playToTreasure game
                    | snd shifted = moveToTreasure $ fst shifted
                    | (snd shiftedNoTreasure) = moveToOnePos (fst shiftedNoTreasure) nearPos
                    | otherwise = Game.movePlayerTo (fst shifted) (head (reachable game))
                    where
                        shifted = putTile game []
                        shiftedNoTreasure = putTile game nearPos
                        treasPos = Game.neededTreasurePos game
                        nearPos = (nearPositionList treasPos 1) ++ (nearPositionList treasPos 2) ++ (nearPositionList treasPos 3)
                        reachable (Game.Game (p:ps) board _) = Game.reachablePosPlayer board p


    -- Shifting tiles

    putTile :: Game.Game -> [Players.Position] -> (Game.Game,Bool)
    putTile game objPos
                        | snd left = left
                        | snd right = right
                        | snd top = top
                        | snd bottom = bottom
                        | otherwise =  (fst left, False)
                        where
                             left = putTileInput game objPos "left"
                             right = putTileInput game objPos "right"
                             top = putTileInput game objPos "top"
                             bottom = putTileInput game objPos "bottom"

    putTileInput :: Game.Game -> [Players.Position] -> String -> (Game.Game,Bool)
    putTileInput game objPos input
                            | snd one = one
                            | snd three = three
                            | snd five = five
                            | otherwise = (fst one, False)
                            where
                                one = putTilePosition game objPos input 1
                                three = putTilePosition game objPos input 3
                                five = putTilePosition game objPos input 5


    putTilePosition :: Game.Game -> [Players.Position] -> String -> Int -> (Game.Game,Bool)
    putTilePosition game objPos input pos
                            | snd north = north
                            | snd south = south
                            | snd west = west
                            | snd east = east
                            | otherwise = (fst north, False)
                            where
                                north = putTileDirection game objPos input pos Tiles.North
                                south = putTileDirection game objPos input pos Tiles.South
                                west = putTileDirection game objPos input pos Tiles.West
                                east = putTileDirection game objPos input pos Tiles.East

    putTileDirection :: Game.Game -> [Players.Position] -> String -> Int -> Tiles.Direction -> (Game.Game,Bool)
    putTileDirection game objPos input pos dir = (insertedGame,check)
                            where
                                canReachTreasure = length (Game.reachableTreasureNeeded insertedGame) > 0
                                insertedGame = func (newGame game) pos
                                newGame (Game.Game players board tile) = Game.Game players board (newTile tile)
                                newTile (Tiles.Tile k t _) = Tiles.Tile k t dir
                                func
                                    | input == "top" = Game.insertTop
                                    | input == "bottom" = Game.insertBottom
                                    | input == "left" = Game.insertLeft
                                    | otherwise = Game.insertRight
                                check
                                    | objPos == [] = canReachTreasure
                                    | otherwise = isOnePosReachable insertedGame objPos

    isOnePosReachable :: Game.Game -> [Players.Position] -> Bool
    isOnePosReachable _ [] = False
    isOnePosReachable (Game.Game (p:ps) board tile) ((x,y):xs)
                    | (x,y) `elem` reachables = True
                    | otherwise = isOnePosReachable (Game.Game (p:ps) board tile) xs
                    where
                        reachables = Game.reachablePosPlayer board p


    -- Move pawn

    moveToTreasure :: Game.Game -> Game.Game
    moveToTreasure game = Game.movePlayerTo game treasPos
                    where
                        treasPos = head $ Game.reachableTreasureNeededPos game

    moveToOnePos :: Game.Game -> [Players.Position] -> Game.Game
    moveToOnePos game [] = game
    moveToOnePos game (p:ps)
                | p `elem` reachable game = Game.movePlayerTo game p
                | otherwise = moveToOnePos game ps
                where
                    reachable (Game.Game (p:ps) board _) = Game.reachablePosPlayer board p


    -- others

    nearPositionList :: [Players.Position] -> Int -> [Players.Position]
    nearPositionList [] _ = []
    nearPositionList (p:ps) distance = (nearPosition p distance)++nearPositionList ps distance

    nearPosition  :: Players.Position -> Int -> [Players.Position]
    nearPosition (x,y) distance = [(nx,ny) | nx<-[0..6], ny<-[0..6], (dx,dy)<-distList , nx == 0+dx, ny == 0+dy]
                where
                    distList = [(dx,dy) | dx <- [-6..6], dy <- [-6..6],((abs dx)+ (abs dy)) == distance]
