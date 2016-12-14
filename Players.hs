module Players where
    
    data Color = Yellow | Red | Blue | Green deriving (Enum,Eq,Show)
    data Control = Human | AI deriving (Show)
    type Position = (Int, Int)
    type Card = Int

    data Player = Player {
                  color :: Color,
                  control :: Control,
                  pos :: Position,
                  cards :: [Card]
                  } deriving (Show)


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