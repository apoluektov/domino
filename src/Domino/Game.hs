-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Game where

type Tile = (Int, Int)

data Line = Line [Tile] Int Int
          | Empty
            deriving (Show)

data End = R | L
           deriving (Show, Read, Eq)

type Hand = [Tile]

data Move = Move Tile End
            deriving (Show, Read, Eq)

data GeneralTile = Unknown | Known Tile
                   deriving (Show, Eq, Read)

data Event = EBegin Hand Player -- id of player that makes first move
           | EMove Move
           | EDraw GeneralTile
           | EPass
             deriving (Show, Read, Eq)

type GameEvents = [(Player, Event)]

data Player = Me | Opponent
              deriving (Show, Read, Eq)

data Strategy = Strategy (GameEvents -> (Event, Strategy))

data GameResult = GRDraw | GRWin Player
                deriving (Show, Eq)

isCorrectMove :: Line -> Move -> Bool
isCorrectMove Empty _ = True
isCorrectMove (Line _ p1 _) (Move (x1,x2) L)
    | p1 == x1 || p1 == x2 = True
isCorrectMove (Line _ _ p2) (Move (x1,x2) R)
    | p2 == x1 || p2 == x2 = True
isCorrectMove _ _ = False

ends :: Line -> (Int, Int)
ends (Line _ a b) = (a,b)

-- TODO: make me total
makeMove :: Line -> Move -> Line
makeMove Empty (Move (x0,x1) _) = Line [(x0,x1)] x0 x1
makeMove (Line ps p0 p1) (Move (x0,x1) L)
    | p0 == x0 = (Line ((x1,x0):ps) x1 p1)
    | p0 == x1 = (Line ((x0,x1):ps) x0 p1)
makeMove (Line ps p0 p1) (Move (x0,x1) R)
    | p1 == x0 = (Line (ps ++ [(x0,x1)]) p0 x1)
    | p1 == x1 = (Line (ps ++ [(x1,x0)]) p0 x0)
makeMove _ _ = error "Incorrect move"

correctMoves :: Hand -> Line -> [Move]
correctMoves ps t = [Move p d | p <- ps, d <- [L,R], isCorrectMove t (Move p d)]
