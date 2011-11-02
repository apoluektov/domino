module Game where

type Piece = (Int, Int)

data Table = Table [Piece] Int Int
           | Empty
             deriving (Show)

data Direction = R | L
                 deriving (Show)

isCorrectMove :: Table -> Piece -> Bool
isCorrectMove Empty _ = True
isCorrectMove (Table _ p1 p2) (x1,x2)
    | p1 == x1 || p1 == x2 = True
    | p2 == x1 || p2 == x2 = True
    | otherwise            = False

-- TODO: make me total
makeMove :: Table -> Piece -> Direction -> Table
makeMove Empty (x0,x1) _ = Table [(x0,x1)] x0 x1
makeMove (Table ps p0 p1) (x0,x1) L
    | p0 == x0 = (Table ((x1,x0):ps) x1 p1)
    | p0 == x1 = (Table ((x0,x1):ps) x0 p1)
    | otherwise = error "Incorrect move"
makeMove (Table ps p0 p1) (x0,x1) R
    | p1 == x0 = (Table (ps ++ [(x0,x1)]) p0 x1)
    | p1 == x1 = (Table (ps ++ [(x1,x0)]) p0 x0)
    | otherwise = error "Incorrect move"
