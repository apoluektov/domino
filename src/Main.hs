-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

import Domino.Game
import Domino.GameState
import Domino.Read
import Domino.Strategy
import Domino.Strategy.Simple
import Domino.Strategy.Counting

data GameResult = GRDraw | GRWin Player
                deriving (Show, Eq)

main = do
  result <- game
  putStrLn $ show result

game :: IO GameResult
game = do
  putStrLn "What is my hand?"
  hand <-  readHand
  putStrLn "Whose move is the first?"
  first <- readFirst
  loop first counting [(first, EBegin hand first)] initialState


loop :: Player -> Strategy -> GameEvents -> GameState -> IO GameResult
loop Opponent s evts st = do
  putStrLn "What is opponent's move?"
  move <- readMove
  case move of
    EDraw Unknown -> loop Opponent s ((Opponent,EDraw Unknown):evts) (updateGameState (Opponent,move) st)
    EPass | head evts == (Me,EPass) -> return GRDraw
          | otherwise -> loop Me s ((Opponent,EPass):evts) (updateGameState (Opponent,move) st)
    EMove m | not (isCorrectMove (line st) m) -> do
                putStrLn "Move is not correct; try again:"
                loop Opponent s evts st
            | checkWin Opponent updEvts (updateGameState (Opponent,move) st) -> return (GRWin Opponent)
            | otherwise -> loop Me s updEvts (updateGameState (Opponent,move) st)
      where updEvts = ((Opponent,EMove m):evts)
loop Me (Strategy f) evts st = do
  let (evt, newS) = f evts
  putStrLn $ show evt
  case evt of
     EDraw Unknown -> do
             putStrLn "What did I get from the stock?"
             tile <- readTile
             loop Me newS ((Me,EDraw (Known tile)):evts) (updateGameState (Me,evt) st)
     EPass | head evts == (Opponent,EPass) -> return GRDraw
           | otherwise -> loop Opponent newS ((Me,EPass):evts) (updateGameState (Me,evt) st)
     EMove m | checkWin Me updEvts (updateGameState (Me,evt) st) -> return (GRWin Me)
             | otherwise -> loop Opponent newS updEvts (updateGameState (Me,evt) st)
       where updEvts = ((Me,EMove m):evts)


checkWin :: Player -> GameEvents -> GameState -> Bool
checkWin p evts st = numTiles p st == 0
    where numTiles Me       = length . hand
          numTiles Opponent = opponentHand
