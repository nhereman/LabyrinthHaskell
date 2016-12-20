module Players where
    
    data Color = Yellow | Red | Blue | Green deriving (Enum,Eq)
    data Control = Human | AI deriving (Eq)
    type Position = (Int, Int)
    type Card = Int

    data Player = Player {
                  color :: Color,
                  control :: Control,
                  pos :: Position,
                  cards :: [Card]
                  } deriving (Eq)

    -- Instance Show
    instance Show Color where
        show color
            | color == Yellow = "yellow"
            | color == Red = "red"
            | color == Blue = "blue"
            | otherwise = "green" 

    instance Show Control where
        show ctrl
            | ctrl == Human = "human"
            | otherwise = "ai"

    instance Show Player where
          show (Player color ctrl pos cards) = show color ++ " " ++ show ctrl ++ " " ++ showPos pos ++ " " ++ showCards cards

    showPos :: Position -> String
    showPos (x,y) = show x ++ " " ++ show y

    showCards :: [Card] -> String
    showCards [] = ""
    showCards (c:cs)
        | cs == [] = show c
        | otherwise = show c ++ " " ++ showCards cs

    showPlayers :: [Player] -> String
    showPlayers [] = ""
    showPlayers (p:ps)
        | ps == [] = show p
        | otherwise = show p ++ " " ++ showPlayers ps

    -- Players generation

    generatePlayers :: Int -> Int -> [Player]
    generatePlayers nbPlayer nbHuman = generatePlayers' nbPlayer nbHuman Yellow

    generatePlayers' :: Int -> Int -> Color -> [Player]
    generatePlayers' nbPlayer nbHuman color
        | nbPlayer == 0 = []
        | nbHuman == 0 = ai:generatePlayers' (nbPlayer-1) 0 (succ color)
        | otherwise = player:generatePlayers' (nbPlayer-1) (nbHuman-1) (succ color)
        where
          player = Player color Human (colorPosition color) []
          ai = Player color AI (colorPosition color) []

    colorPosition :: Color -> Position
    colorPosition color
        | color == Yellow = (0,0)
        | color == Red = (6,0)
        | color == Blue = (6,6)
        | otherwise = (0,6)
