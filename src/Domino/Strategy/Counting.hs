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
                moves = correctMoves (hand st) (table st)
                st = restoreGameState evts
                opHand = restoreOpponentHand evts
                minAmount = minimumBy (comparing opChoices)
                opChoices m = sum $ fst $ unzip $ filter f $ zip opHand [0..6]
                    where f (n,i) = (i == a || i == b)
                          (a,b) = ends $ makeMove (table st) m

oppChoices evts m = sum $ fst $ unzip $ filter f $ zip opHand [0..6]
    where f (n,i) = (i == a || i == b)
          (a,b) = ends $ makeMove (table st) m
          st = restoreGameState evts
          opHand = restoreOpponentHand evts


restoreOpponentHand :: GameEvents -> OpponentHand
restoreOpponentHand evts = fst $ foldr f ((replicate 7 7), initialState) evts
    where f :: (PlayerId, Event) -> (OpponentHand, GameState) -> (OpponentHand, GameState)
          f e@(_, (EBegin h _)) (oh,gs)   = (foldr upd oh h, updateGameState e gs)
          f e@(Opponent, (EMove (Move p _))) (oh,gs) = (upd p oh, updateGameState e gs)
          f e@(Opponent, (EDraw _)) (oh,gs) = (newOh, updateGameState e gs)
              where newOh = map noEnds (zip oh [0..6])
                    noEnds (n,i) | i == a || i == b = 0
                                 | otherwise        = n
                    (a,b) = ends $ table gs
          f e@(Me, (EDraw (Known p))) (oh,gs) = (upd p oh, updateGameState e gs)
          f e (oh,gs) = (oh, updateGameState e gs)
          upd :: Piece -> OpponentHand -> OpponentHand
          upd (a,b) oh = map u (zip oh [0..6])
              where u (n,i) | i == a || i == b = max (n-1) 0
                            | otherwise        = n
