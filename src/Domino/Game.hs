-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Game where

type Piece = (Int, Int)

data Table = Table [Piece] Int Int
           | Empty
             deriving (Show)

data Direction = R | L
                 deriving (Show, Read, Eq)

type Hand = [Piece]

data Move = Move Piece Direction
            deriving (Show, Read, Eq)

data GeneralPiece = Unknown | Known Piece
                    deriving (Show, Eq, Read)

data Event = EBegin Hand PlayerId -- id of player that makes first move
           | EMove Move
           | EDraw GeneralPiece
           | EPass
             deriving (Show, Read, Eq)

type GameEvents = [(PlayerId, Event)]

data PlayerId = Me | Opponent
                deriving (Show, Read, Eq)

data Strategy = Strategy (GameEvents -> (Event, Strategy))

data GameResult = GRDraw | GRWin PlayerId
                deriving (Show, Eq)

isCorrectMove :: Table -> Move -> Bool
isCorrectMove Empty _ = True
isCorrectMove (Table _ p1 _) (Move (x1,x2) L)
    | p1 == x1 || p1 == x2 = True
isCorrectMove (Table _ _ p2) (Move (x1,x2) R)
    | p2 == x1 || p2 == x2 = True
isCorrectMove _ _ = False

ends :: Table -> (Int, Int)
ends (Table _ a b) = (a,b)

-- TODO: make me total
makeMove :: Table -> Move -> Table
makeMove Empty (Move (x0,x1) _) = Table [(x0,x1)] x0 x1
makeMove (Table ps p0 p1) (Move (x0,x1) L)
    | p0 == x0 = (Table ((x1,x0):ps) x1 p1)
    | p0 == x1 = (Table ((x0,x1):ps) x0 p1)
makeMove (Table ps p0 p1) (Move (x0,x1) R)
    | p1 == x0 = (Table (ps ++ [(x0,x1)]) p0 x1)
    | p1 == x1 = (Table (ps ++ [(x1,x0)]) p0 x0)
makeMove _ _ = error "Incorrect move"

correctMoves :: Hand -> Table -> [Move]
correctMoves ps t = [Move p d | p <- ps, d <- [L,R], isCorrectMove t (Move p d)]
