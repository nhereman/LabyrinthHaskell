module Loader where

    import Control.Monad
    import Control.Applicative
    import Data.Char
    import Debug.Trace

    import qualified Game
    import qualified Tiles
    import qualified Players


    -- Parser  base
    newtype Parser a = Parser { parse :: [String] -> [(a,[String])] }

    instance Functor Parser where
        fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

    instance Applicative Parser where
        pure = return
        (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

    instance Monad Parser where
        return x = Parser (\s->[(x,s)])
        p >>= q = Parser (\s->[(y,s'') |
                                (x,s') <- apply p s,
                                (y,s'')<- apply (q x) s'] )

    instance MonadPlus Parser where
        mzero = zero
        mplus = plus

    instance Alternative Parser where
        empty = mzero
        (<|>) = orelse

    apply :: Parser a -> [String] -> [(a,[String])]
    apply (Parser f) s = f s

    applyParser :: Parser a -> [String] -> a
    applyParser p = fst . head . apply p

    item :: Parser String
    item = Parser f where f []     = []
                          f (c:cs) = [(c,cs)]
    zero :: Parser a
    zero = Parser (\s -> [])

    plus :: Parser a -> Parser a -> Parser a
    p `plus` q = Parser (\s -> apply p s ++ apply q s)

    orelse :: Parser a -> Parser a -> Parser a
    p `orelse` q = Parser (\s -> if null (apply p s)
                                 then apply q s
                                 else apply p s)

    sat :: (String -> Bool) -> Parser String
    sat p = do c <- item
               if p c then return c
                      else zero

    pmany :: Parser a -> Parser [a]
    pmany p = do x <- p
                 xs <- pmany p
                 return (x:xs)
            `orelse`
                return []

    psome :: Parser a -> Parser [a]
    psome p = do {x<-p; xs<-many p; return (x:xs)}


    -- Labyrinth parser

    natural :: Parser Int
    natural = do n <- sat (all isDigit)
                 return $ read n

    direction :: Parser Tiles.Direction
    direction = do d <- (sat (isDir) `orelse` return "error")
                   strToDir d
                    where
                        isDir x = x `elem` ["north","east","south","west"]
                        strToDir x
                                | x == "north" = return Tiles.North
                                | x == "east" = return Tiles.East
                                | x == "south" = return Tiles.South
                                | x == "west" = return Tiles.West
                                | otherwise = zero

    kind :: Parser Tiles.Kind
    kind = do k <- (sat (isKind) `orelse` return "error")
              strToKind k
                where
                    isKind x = x `elem` ["corner", "tshape", "line"]
                    strToKind x
                            | x == "corner" = return Tiles.Corner
                            | x == "tshape" = return Tiles.Tshaped
                            | x == "line" = return Tiles.Line
                            | otherwise = zero

    control :: Parser Players.Control
    control = do c <- (sat (isControl) `orelse` return "error")
                 strToControl c
                where
                    isControl x = (x == "human") || (x == "ai")
                    strToControl x
                                | x == "human" = return Players.Human
                                | x == "ai" = return Players.AI
                                | otherwise = zero

    color :: Parser Players.Color
    color = do c <- (sat (isColor) `orelse` return "error")
               strToColor c
                where
                    isColor x = x `elem` ["yellow","red","blue","green"]
                    strToColor x
                            | x == "yellow" = return Players.Yellow
                            | x == "red" = return Players.Red
                            | x == "blue" = return Players.Blue
                            | x == "green" = return Players.Green
                            | otherwise = zero

    treasure :: Parser Tiles.Treasure
    treasure = do n <- natural
                  return n

    position :: Parser Players.Position
    position = do x <- natural
                  y <- natural
                  return (x,y)


    tile :: Parser Tiles.Tile
    tile = do k <- kind
              t <- (treasure `orelse` return 0)
              d <- direction
              return $ Tiles.Tile k t d

    tiles :: Parser [Tiles.Tile]
    tiles = do t <- (tile `orelse` return Tiles.Empty)
               ts <- if (t == Tiles.Empty) then return [] else tiles
               return (if (t == Tiles.Empty) then ts else (t:ts))

    xtile :: Parser Tiles.Tile
    xtile = do k <- kind
               t <- (treasure `orelse` return 0)
               return $ Tiles.Tile k t Tiles.North

    card :: Parser Players.Card
    card = natural

    cards :: Parser [Players.Card]
    cards = many card

    player :: Parser Players.Player
    player = do col <- color
                ctr <- control
                p <- position
                car <- cards
                return $ Players.Player col ctr p car

    players :: Parser [Players.Player]
    players = many player

    labyrinth :: Parser Game.Game
    labyrinth = do p <- players
                   x <- xtile
                   t <- tiles
                   let board = Game.loadBoard t
                   return $ Game.Game p board x


    -- Game loading
    loadGameFromFile :: String -> IO Game.Game
    loadGameFromFile path = do str <- readFile path
                               let toParse = words str
                               let game = applyParser labyrinth toParse
                               return $ game