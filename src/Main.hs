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
  putStrLn "Which tiles nobody has (revealed during negotiation)?"
  nobodyHas <- readHand
  putStrLn "What is the opening move?"
  firstMove <- readTile
  putStrLn "Whose move is the first?" -- TODO: it can be deduced from revealed
  first <- readFirst
  loop first counting [(first, EBegin hand first nobodyHas firstMove)]
  -- TODO: tiles that nobody has should be taken into account by strategies


loop :: Player -> Strategy -> GameEvents -> IO GameResult
loop Opponent s evts = do
  putStrLn "What is opponent's move?"
  move <- readMove
  case move of
    EDraw Unknown -> loop Opponent s ((Opponent,EDraw Unknown):evts)
    EPass | head evts == (Me,EPass) -> return GRDraw
          | otherwise -> loop Me s ((Opponent,EPass):evts)
    EMove m | not (isCorrectMove (line (restoreGameState evts)) m) -> do
                putStrLn "Move is not correct; try again:"
                loop Opponent s evts
            | checkWin Opponent updEvts -> return (GRWin Opponent)
            | otherwise -> loop Me s updEvts
      where updEvts = ((Opponent,EMove m):evts)
loop Me (Strategy f) evts = do
  let (evt, newS) = f evts
  putStrLn $ show evt
  case evt of
     EDraw Unknown -> do
             putStrLn "What did I get from the stock?"
             tile <- readTile
             loop Me newS ((Me,EDraw (Known tile)):evts)
     EPass | head evts == (Opponent,EPass) -> return GRDraw
           | otherwise -> loop Opponent newS ((Me,EPass):evts)
     EMove m | checkWin Me updEvts -> return (GRWin Me)
             | otherwise -> loop Opponent newS updEvts
       where updEvts = ((Me,EMove m):evts)


checkWin :: Player -> GameEvents -> Bool
checkWin p evts = numTiles p st == 0
    where numTiles Me       = length . hand
          numTiles Opponent = opponentHand
          st = restoreGameState evts
