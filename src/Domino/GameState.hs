-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanying file MIT-LICENSE)

module Domino.GameState where

import Domino.Game

data GameState = GameState {
      stock        :: Int
    , opponentHand :: Int
    , hand         :: Hand
    , line         :: Line
    , events       :: GameEvents
    } deriving (Show)

initialState :: GameState
initialState = GameState 28 0 [] Empty []

updateGameState :: (Player,Event) -> GameState -> GameState
updateGameState e@(_, (EBegin h _ _ _)) st = (GameState (28 - 7 - length h) 7 h Empty [e])
updateGameState e@(Opponent, (EMove m)) (GameState stk oh h t es)
    = (GameState stk (oh-1) h (makeMove t m) (e:es))
updateGameState e@(Me, (EMove m@(Move p _))) (GameState stk oh h t es)
    = GameState stk oh newH (makeMove t m) (e:es)
      where newH = filter (neq p) h
            neq (a,b) (x,y) = ((a,b) /= (x,y) && (a,b) /= (y,x))
updateGameState e@(Opponent,(EDraw _)) (GameState stk oh h t es)
    = GameState (stk-1) (oh+1) h t (e:es)
updateGameState e@(Me, (EDraw (Known p))) (GameState stk oh h t es)
    = GameState (stk-1) oh (p:h) t (e:es)
updateGameState e@(_, EPass) gs = gs { events = (e:events gs) }


restoreGameState :: GameEvents -> GameState
restoreGameState evts = foldr updateGameState initialState evts
