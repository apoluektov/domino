-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanying file MIT-LICENSE)

module Domino.Strategy.Simple 
    (
      simpleStrat
    ) where

import Domino.Game
import Domino.GameState
import Domino.Strategy

anyCorrectMove :: GameState -> Event
anyCorrectMove st = evt st
    where evt st =
              case events st of
                [(Me, EBegin _ Me _ firstTile)] -> EMove (Move firstTile L)
                _
                    | null moves && stock st > 0 -> EDraw Unknown
                    | null moves                 -> EPass
                    | otherwise                  -> EMove (head moves)
              where moves = correctMoves (hand st) (line st)

update :: GameState -> (Player, Event) -> Strategy
update st e = Strategy update anyCorrectMove (updateGameState e st)

simpleStrat :: Strategy
simpleStrat = Strategy update anyCorrectMove initialState
