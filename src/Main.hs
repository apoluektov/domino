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

import Control.Monad.State

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
  evalStateT (updatedLoop first counting e) initialState


type StateGameState = StateT GameState IO

update :: (Player, Event) -> StateGameState ()
update evt = do
  modify $ updateGameState evt

-- TODO: clean me
loop :: Player -> Strategy -> StateGameState GameResult
loop Opponent s = do
  lift $ putStrLn "What is opponent's move?"
  st <- get
  move <- lift $ readMove
  case move of
    EDraw Unknown -> updatedLoop Opponent s (Opponent,move)
    EPass | head (events st) == (Me,EPass) -> return GRDraw
          | otherwise -> updatedLoop Me s (Opponent,move)
    EMove m | not (isCorrectMove (line st) m) -> do
                lift $ putStrLn "Move is not correct; try again:"
                loop Opponent s
            | checkWin Opponent (updateGameState (Opponent,move) st) -> return (GRWin Opponent)
            | otherwise -> updatedLoop Me s (Opponent,move)
loop Me s = do
  let evt = next s
  st <- get
  lift $ putStrLn $ show evt
  case evt of
     EDraw Unknown -> do
             lift $ putStrLn "What did I get from the stock?"
             tile <- lift $ readTile
             updatedLoop Me s (Me,EDraw $ Known tile)
     EPass | head (events st) == (Opponent,EPass) -> return GRDraw
           | otherwise -> updatedLoop Opponent s (Me,evt)
     EMove m | checkWin Me (updateGameState (Me,evt) st) -> return (GRWin Me)
             | otherwise -> updatedLoop Opponent s (Me,evt)

updatedLoop :: Player -> Strategy -> (Player,Event) -> StateGameState GameResult
updatedLoop p s e = update e >> loop p (notify s e)

checkWin :: Player -> GameState -> Bool
checkWin p st = numTiles p st == 0
    where numTiles Me       = length . hand
          numTiles Opponent = opponentHand
