-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanying file MIT-LICENSE)

module Domino.Strategy.Counting
    (
      counting
    ) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Domino.Game
import Domino.GameState
import Domino.Strategy

type MaxAmount = Int
type OpponentHand = [MaxAmount]

counting :: Strategy
counting = Strategy updateInfo mostInconvenientMove (initialOpponentHand,initialState)

mostInconvenientMove :: (OpponentHand, GameState) -> Event
mostInconvenientMove (opHand,st) =
    case events st of
      [(Me, EBegin _ Me _ firstTile)] -> EMove (Move firstTile L)
      _
          | null moves && stock st > 0 -> EDraw Unknown
          | null moves                 -> EPass
          | otherwise                  -> EMove (minAmount moves)
    where
      moves = correctMoves (hand st) (line st)
      minAmount = minimumBy (comparing opChoices)
      opChoices m = sum $ fst $ unzip $ filter f $ zip opHand [0..6]
          where f (n,i) = (i == a || i == b)
                (a,b) = ends $ makeMove (line st) m

updateInfo :: (OpponentHand, GameState) -> (Player, Event) -> Strategy
updateInfo s@(hand, st) e = Strategy updateInfo mostInconvenientMove updState
    where updState = (updateOpponentHandFromEvent e s, updateGameState e st)

restoreOpponentHand :: GameEvents -> OpponentHand
restoreOpponentHand = fst . foldr f (initialOpponentHand, initialState)
    where f e (oh, st)
              = (updateOpponentHandFromEvent e (oh,st), updateGameState e st)

updateOpponentHandFromEvent :: (Player, Event)
                            -> (OpponentHand, GameState)
                            -> OpponentHand
updateOpponentHandFromEvent (_, (EBegin h _ _ _)) (oh,gs)
    = foldr updateOpponentHand oh h
-- TODO: take tile that nobody has into account
updateOpponentHandFromEvent (Opponent, (EMove (Move p _))) (oh,gs)
    = updateOpponentHand p oh
updateOpponentHandFromEvent (Opponent, (EDraw _)) (oh,gs) = newOh
    where newOh = map noEnds (zip oh [0..6])
          noEnds (n,i) | i == a || i == b = 0
                       | otherwise        = n
          (a,b) = ends $ line gs
updateOpponentHandFromEvent (Me, (EDraw (Known p))) (oh,gs)
    = updateOpponentHand p oh
updateOpponentHandFromEvent _ (oh, _) = oh

updateOpponentHand :: Tile -> OpponentHand -> OpponentHand
updateOpponentHand (a,b) oh = map u (zip oh [0..6])
    where u (n,i) | i == a || i == b = max (n-1) 0
                  | otherwise        = n

initialOpponentHand :: OpponentHand
initialOpponentHand = (replicate 7 7)
