-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

import Domino.Game
import Domino.GameState
import Domino.Read
import Domino.Strategy.Simple
import Domino.Strategy.Counting

main = do
  result <- game
  putStrLn $ show result

game :: IO GameResult
game = do
  putStrLn "What is my hand?"
  hand <-  readHand
  putStrLn "Whose move is the first?"
  first <- readFirst
  loop first counting [(first, EBegin hand first)]


loop :: PlayerId -> Strategy -> GameEvents -> IO GameResult
loop Opponent s evts = do
  putStrLn "What is opponent's move?"
  move <- readMove
  case move of
    EDraw Unknown -> loop Opponent s ((Opponent,EDraw Unknown):evts)
    EPass | head evts == (Me,EPass) -> return GRDraw
          | otherwise -> loop Me s ((Opponent,EPass):evts)
    EMove m | checkWin Opponent updEvts -> return (GRWin Opponent)
            | otherwise -> loop Me s updEvts
      where updEvts = ((Opponent,EMove m):evts)
loop Me (Strategy f) evts = do
  let (evt, newS) = f evts
  putStrLn $ show evt
  case evt of
     EDraw Unknown -> do
             putStrLn "What did I get from the stock?"
             piece <- readPiece
             loop Me newS ((Me,EDraw (Known piece)):evts)
     EPass | head evts == (Opponent,EPass) -> return GRDraw
           | otherwise -> loop Opponent newS ((Me,EPass):evts)
     EMove m | checkWin Me updEvts -> return (GRWin Me)
             | otherwise -> loop Opponent newS updEvts
       where updEvts = ((Me,EMove m):evts)


checkWin :: PlayerId -> GameEvents -> Bool
checkWin p evts = numPieces p st == 0
    where numPieces Me       = length . hand
          numPieces Opponent = opponentHand
          st = restoreGameState evts
