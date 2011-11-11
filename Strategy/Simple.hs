module Strategy.Simple where

import Game
import GameState

simpleStrat :: Strategy
simpleStrat = (Strategy f)
    where f evts = (evt, (Strategy f))
              where
                evt
                    | null moves && stock st > 0 = EDraw Unknown
                    | null moves              = EPass
                    | otherwise               = EMove (head moves)
                moves = correctMoves (hand st) (table st)
                st = restoreGameState evts
