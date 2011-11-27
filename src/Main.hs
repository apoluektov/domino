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
  let e = (first, EBegin hand first)
  loop first (notify simpleStrat e) (updateGameState e initialState)


loop :: Player -> Strategy -> GameState -> IO GameResult
loop Opponent s st = do
  putStrLn "What is opponent's move?"
  move <- readMove
  case move of
    EDraw Unknown -> loop Opponent (notify s (Opponent,move)) (updateGameState (Opponent,move) st)
    EPass | head (events st) == (Me,EPass) -> return GRDraw
          | otherwise -> loop Me (notify s (Opponent,move)) (updateGameState (Opponent,move) st)
    EMove m | not (isCorrectMove (line st) m) -> do
                putStrLn "Move is not correct; try again:"
                loop Opponent s st
            | checkWin Opponent (updateGameState (Opponent,move) st) -> return (GRWin Opponent)
            | otherwise -> loop Me (notify s (Opponent,move)) (updateGameState (Opponent,move) st)
loop Me s st = do
  let evt = next s
  putStrLn $ show evt
  case evt of
     EDraw Unknown -> do
             putStrLn "What did I get from the stock?"
             tile <- readTile
             loop Me (notify s (Me,EDraw $ Known tile)) (updateGameState (Me,EDraw $ Known tile) st)
     EPass | head (events st) == (Opponent,EPass) -> return GRDraw
           | otherwise -> loop Opponent (notify s (Me,evt)) (updateGameState (Me,evt) st)
     EMove m | checkWin Me (updateGameState (Me,evt) st) -> return (GRWin Me)
             | otherwise -> loop Opponent (notify s (Me,evt)) (updateGameState (Me,evt) st)

checkWin :: Player -> GameState -> Bool
checkWin p st = numTiles p st == 0
    where numTiles Me       = length . hand
          numTiles Opponent = opponentHand
