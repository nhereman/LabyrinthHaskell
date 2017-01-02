module AI where

    import qualified Tiles
    import qualified Players
    import qualified Game

    -- Tile insertion

    computeGameScore :: Game.Game -> Int
    computeGameScore game
                        | nbCards game == 0 && startReachable game = 1000
                        | otherwise = 24 - (length (Game.reachableTreasureNeeded game))
                        where
                            nbCards (Game.Game ((Players.Player _ _ _ cards):_) _ _) = length cards
                            startReachable (Game.Game ((Players.Player col _ pos _):_) board _) = (Players.colorPosition col)
                                                                                                   `elem` Game.reachablePos board pos []

    getBestGame :: [Game.Game] -> (Game.Game,Int)
    getBestGame (g:[]) = (g, computeGameScore g)
    getBestGame (g:gs) = let best = getBestGame gs
                             score = computeGameScore g in
                         if snd best >= score then
                            best
                         else
                            (g,score)

    getBestGameComputed :: [(Game.Game, Int)] -> (Game.Game,Int)
    getBestGameComputed (g:[]) = g
    getBestGameComputed ((ga,s):gs) = let best = getBestGameComputed gs in
                                      if snd best >= s then
                                        best
                                      else
                                        (ga,s)


    
    bestMove :: Game.Game -> Game.Game
    bestMove game = fst $ getBestGameComputed [left,right,top,bottom]
            where
                left = bestPosMove game "left"
                right = bestPosMove game "right"
                top = bestPosMove game "top"
                bottom = bestPosMove game "bottom"


    bestPosMove :: Game.Game -> String -> (Game.Game,Int)
    bestPosMove game input = getBestGameComputed [one,three,five]
            where
                one = bestDirMove game input 1
                three = bestDirMove game input 3
                five = bestDirMove game input 5

    bestDirMove :: Game.Game -> String -> Int -> (Game.Game,Int)
    bestDirMove (Game.Game players board (Tiles.Tile k t _)) input pos = getBestGame [north,south,east,west]
            where
                north = insertfunc (newgame Tiles.North) pos
                south = insertfunc (newgame Tiles.South) pos
                east = insertfunc (newgame Tiles.East) pos
                west = insertfunc (newgame Tiles.West) pos
                insertfunc
                    | input == "top" = Game.insertTop
                    | input == "bottom" = Game.insertBottom
                    | input == "left" = Game.insertLeft
                    | otherwise = Game.insertRight
                newtile dir = (Tiles.Tile k t dir)
                newgame dir = Game.Game players board $ newtile dir



    -- Pawn movement

    movePawn :: Game.Game -> Game.Game
    movePawn game = Game.movePlayerTo game $ fst $ bestPosition game $ reachable game
            where
                reachable (Game.Game (p:_) b _) = Game.reachablePosPlayer b p

    bestPosition :: Game.Game -> [Players.Position] -> (Players.Position,Int)
    bestPosition game (p:[]) = (p,positionScore game p)
    bestPosition game (p:ps) =  if (snd best) > score then
                                    best
                                else
                                    (p,score)
                                where
                                    score = positionScore game p
                                    best = bestPosition game ps

    positionScore :: Game.Game -> Players.Position -> Int
    positionScore game pos
                    | (nbCards game == 0) = 100 - distToStart
                    | otherwise = 50 - (shortestTreasureDist pos $ treasurePos game 0 0)
                    where
                        nbCards (Game.Game ((Players.Player _ _ _ cards):_) _ _) = length cards
                        start (Game.Game ((Players.Player col _ _ _):_) _ _) = Players.colorPosition col
                        distToStart = distToPos pos $ start game

    distToPos :: Players.Position -> Players.Position -> Int
    distToPos pos1 pos2 = (abs ((fst pos1)-(fst pos2))) + (abs ((snd pos1)-(snd pos2)))

    treasurePos :: Game.Game -> Int -> Int -> [Players.Position]
    treasurePos game col row
                    | col == 7 && row == 6 = []
                    | col == 7 = treasurePos game 0 (row+1)
                    | getTreasure game `elem` cards game = (col,row):treasurePos game (col+1) row
                    | otherwise = treasurePos game (col+1) row
                    where
                        cards (Game.Game (p:_) _ _) = Players.cards p
                        getTreasure (Game.Game _ board _) = Tiles.treasure $ Game.getBoardTile board col row

    shortestTreasureDist :: Players.Position -> [Players.Position] -> Int
    shortestTreasureDist p1 [] = 0
    shortestTreasureDist p1 (p2:[]) = distToPos p1 p2
    shortestTreasureDist p1 (p2:p2s) =  if dist > short then
                                           short
                                        else
                                           dist
                                    where
                                        dist = distToPos p1 p2
                                        short = shortestTreasureDist p1 p2s





