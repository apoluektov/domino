-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Strategy.Simple where

import Domino.Game
import Domino.GameState
import Domino.Strategy
import Control.Monad.State

anyCorrectMove :: State GameState Event
anyCorrectMove = do
  st <- get
  let e = evt st
  modify $ updateGameState (Me, e)
  return e
    where evt st
              | null moves && stock st > 0 = EDraw Unknown
              | null moves                 = EPass
              | otherwise                  = EMove (head moves)
              where moves = correctMoves (hand st) (line st)

simpleStrat :: Strategy
simpleStrat = statelessStrategy f
    where f :: GameState -> Event
          f state = evalState anyCorrectMove state
