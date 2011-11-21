-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Strategy.Simple where

import Domino.Game
import Domino.GameState
import Domino.Strategy

anyCorrectMove :: GameEvents -> Event
anyCorrectMove evts
    | null moves && stock st > 0 = EDraw Unknown
    | null moves                 = EPass
    | otherwise                  = EMove (head moves)
    where
      moves = correctMoves (hand st) (line st)
      st = restoreGameState evts

simpleStrat :: Strategy
simpleStrat = statelessStrategy anyCorrectMove
