import Control.Exception
import Game

main = do
  result <- game
  putStrLn $ show result

game :: IO GameResult
game = do
  putStrLn "What is my hand?"
  hand <-  readHand
  putStrLn "Whose move is the first?"
  first <- readFirst
  loop first simpleStrat [(first, EBegin hand first)]

data GameState = GameState Int Int Hand Table
                 deriving (Show)
-- pieces in stock; pieces at opponent's hand; my hand; table

simpleStrat :: Strategy
simpleStrat = (Strategy f)
    where f evts = (evt, (Strategy f))
              where
                evt
                    | null moves && stock > 0 = EDraw Unknown
                    | null moves              = EPass
                    | otherwise               = EMove (head moves)
                moves = correctMoves hand table
                (GameState stock _ hand table) = restoreGameState evts

restoreGameState :: GameEvents -> GameState
restoreGameState evts = foldr f (GameState 28 0 [] Empty) evts
    where f :: (PlayerId,Event) -> GameState -> GameState
          f (_, (EBegin h _)) st = (GameState 14 7 h Empty)
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

readHand :: IO Hand
readHand = readLn

readFirst :: IO PlayerId
readFirst = readLn

readMove :: IO Event
readMove = readLn

loop :: PlayerId -> Strategy -> GameEvents -> IO GameResult
loop Opponent s evts = do
  putStrLn "What is opponent's move?"
  move <- readLn
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
     -- TODO: should be notified of what was drawn
     EDraw Unknown -> do
             putStrLn "What did I get from the stock?"
             piece <- readLn
             loop Me newS ((Me,EDraw (Known piece)):evts)
     EPass | head evts == (Opponent,EPass) -> return GRDraw
           | otherwise -> loop Opponent newS ((Me,EPass):evts)
     EMove m | checkWin Me updEvts -> return (GRWin Me)
             | otherwise -> loop Opponent newS updEvts
       where updEvts = ((Me,EMove m):evts)


-- TODO: restore game state from events
-- TODO: parsing errors should not lead to crash

-- TODO: refactor me
checkWin :: PlayerId -> GameEvents -> Bool
checkWin p evts = checkWin' evts 7 == 0
    where checkWin' [] acc = acc
          checkWin' ((p',EDraw _):evts) acc
              | p == p'   = checkWin' evts acc+1
              | otherwise = checkWin' evts acc
          checkWin' ((p',EMove _):evts) acc
              | p == p'   = checkWin' evts acc-1
              | otherwise = checkWin' evts acc
          checkWin' (_:evts) acc = checkWin' evts acc
