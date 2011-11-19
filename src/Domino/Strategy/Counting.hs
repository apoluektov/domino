-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Strategy.Counting where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Domino.Game
import Domino.GameState

type MaxAmount = Int
type OpponentHand = [MaxAmount]

counting :: Strategy
counting = (Strategy f)
    where f evts = (evt, (Strategy f))
              where
                evt
                    | null moves && stock st > 0 = EDraw Unknown
                    | null moves              = EPass
                    | otherwise               = EMove (minAmount moves)
                moves = correctMoves (hand st) (line st)
                st = restoreGameState evts
                opHand = restoreOpponentHand evts
                minAmount = minimumBy (comparing opChoices)
                opChoices m = sum $ fst $ unzip $ filter f $ zip opHand [0..6]
                    where f (n,i) = (i == a || i == b)
                          (a,b) = ends $ makeMove (line st) m

restoreOpponentHand :: GameEvents -> OpponentHand
restoreOpponentHand evts = fst $ foldr f (initialOpponentHand, initialState) evts
    where f :: (Player, Event) -> (OpponentHand, GameState) -> (OpponentHand, GameState)
          f e@(_, (EBegin h _)) (oh,gs)
              = (foldr updateOpponentHand oh h, updateGameState e gs)
          f e@(Opponent, (EMove (Move p _))) (oh,gs)
              = (updateOpponentHand p oh, updateGameState e gs)
          f e@(Opponent, (EDraw _)) (oh,gs) = (newOh, updateGameState e gs)
              where newOh = map noEnds (zip oh [0..6])
                    noEnds (n,i) | i == a || i == b = 0
                                 | otherwise        = n
                    (a,b) = ends $ line gs
          f e@(Me, (EDraw (Known p))) (oh,gs)
              = (updateOpponentHand p oh, updateGameState e gs)
          f e (oh,gs) = (oh, updateGameState e gs)


updateOpponentHand :: Tile -> OpponentHand -> OpponentHand
updateOpponentHand (a,b) oh = map u (zip oh [0..6])
    where u (n,i) | i == a || i == b = max (n-1) 0
                  | otherwise        = n

initialOpponentHand :: OpponentHand
initialOpponentHand = (replicate 7 7)
