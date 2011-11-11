module GameState where

import Game

data GameState = GameState {
      stock        :: Int
    , opponentHand :: Int
    , hand         :: Hand
    , table        :: Table
    } deriving (Show)


restoreGameState :: GameEvents -> GameState
restoreGameState evts = foldr f (GameState 28 0 [] Empty) evts
    where f :: (PlayerId,Event) -> GameState -> GameState
          f (_, (EBegin h _)) st = (GameState (28 - length h) 7 h Empty)
          f (Opponent, (EMove m)) (GameState stk oh h t)
              = (GameState stk (oh-1) h (makeMove t m))
          f (Me, (EMove m@(Move p _))) (GameState stk oh h t)
              = GameState stk oh newH (makeMove t m)
                where newH = filter (neq p) h
                      neq (a,b) (x,y) = ((a,b) /= (x,y) && (a,b) /= (y,x))
          f (Opponent,(EDraw _)) (GameState stk oh h t)
              = GameState (stk-1) (oh+1) h t
          f (Me, (EDraw (Known p))) (GameState stk oh h t)
              = GameState (stk-1) oh (p:h) t
          f (_, EPass) gs = gs


