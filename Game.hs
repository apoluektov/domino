module Game where

type Piece = (Int, Int)

data Table = Table [Piece] Int Int
           | Empty
             deriving (Show)

isCorrectMove :: Table -> Piece -> Bool
isCorrectMove Empty _ = True
isCorrectMove (Table _ p1 p2) (x1,x2)
    | p1 == x1 || p1 == x2 = True
    | p2 == x1 || p2 == x2 = True
    | otherwise            = False
