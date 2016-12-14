module Players where
    
    data Color = Yellow | Red | Blue | Green
    data Control = Human | AI
    type Position = (Int, Int)
    type Card = Int

    data Player = Player {
                  color :: Color,
                  control :: Control,
                  pos :: Position,
                  cards :: [Card]
                  }