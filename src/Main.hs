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
  evalStateT (e >>> loop first) (initialState, counting)


type StateGameState = StateT (GameState, Strategy) IO

loop :: Player -> StateGameState GameResult
loop Opponent = do
  lift $ putStrLn "What is opponent's move?"
  (st,_) <- get
  move <- lift $ readMove
  case move of
    EDraw Unknown -> (Opponent,move) >>> loop Opponent
    EPass | head (events st) == (Me,EPass) -> return GRDraw
          | otherwise -> (Opponent,move) >>> loop Me
    EMove m | not (isCorrectMove (line st) m) -> do
                lift $ putStrLn "Move is not correct; try again:"
                loop Opponent
            | checkWin Opponent (updateGameState (Opponent,move) st) -> return (GRWin Opponent)
            | otherwise -> (Opponent,move) >>> loop Me
loop Me = do
  (st, s) <- get
  let evt = next s
  lift $ putStrLn $ show evt
  case evt of
     EDraw Unknown -> do
             lift $ putStrLn "What did I get from the stock?"
             tile <- lift $ readTile
             (Me,EDraw $ Known tile) >>> loop Me
     EPass | head (events st) == (Opponent,EPass) -> return GRDraw
           | otherwise -> (Me,evt) >>> loop Opponent
     EMove m | checkWin Me (updateGameState (Me,evt) st) -> return (GRWin Me)
             | otherwise -> (Me,evt) >>> loop Opponent

(>>>) :: (Player,Event) -> StateGameState GameResult -> StateGameState GameResult
evt >>> st = (modify $ \(st,s) -> (updateGameState evt st, notify s evt)) >> st

checkWin :: Player -> GameState -> Bool
checkWin p st = numTiles p st == 0
    where numTiles Me       = length . hand
          numTiles Opponent = opponentHand
