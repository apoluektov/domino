-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.GameState where

import Domino.Game

data GameState = GameState {
      stock        :: Int
    , opponentHand :: Int
    , hand         :: Hand
    , table        :: Table
    } deriving (Show)

initialState :: GameState
initialState = GameState 28 0 [] Empty

updateGameState :: (PlayerId,Event) -> GameState -> GameState
updateGameState (_, (EBegin h _)) st = (GameState (28 - length h) 7 h Empty)
updateGameState (Opponent, (EMove m)) (GameState stk oh h t)
    = (GameState stk (oh-1) h (makeMove t m))
updateGameState (Me, (EMove m@(Move p _))) (GameState stk oh h t)
    = GameState stk oh newH (makeMove t m)
      where newH = filter (neq p) h
            neq (a,b) (x,y) = ((a,b) /= (x,y) && (a,b) /= (y,x))
updateGameState (Opponent,(EDraw _)) (GameState stk oh h t)
    = GameState (stk-1) (oh+1) h t
updateGameState (Me, (EDraw (Known p))) (GameState stk oh h t)
    = GameState (stk-1) oh (p:h) t
updateGameState (_, EPass) gs = gs


restoreGameState :: GameEvents -> GameState
restoreGameState evts = foldr updateGameState initialState evts
