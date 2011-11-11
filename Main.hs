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

data GameState = GameState {
      stock        :: Int
    , opponentHand :: Int
    , hand         :: Hand
    , table        :: Table
    } deriving (Show)

simpleStrat :: Strategy
simpleStrat = (Strategy f)
    where f evts = (evt, (Strategy f))
              where
                evt
                    | null moves && stock st > 0 = EDraw Unknown
                    | null moves              = EPass
                    | otherwise               = EMove (head moves)
                moves = correctMoves (hand st) (table st)
                st = restoreGameState evts

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

readUserInput :: (Read a) => String -> IO a
readUserInput retryMsg = do
  s <- getLine
  let r = reads s
  case r of
    [(i,"")] -> return i
    _ -> do
           putStrLn retryMsg
           readUserInput retryMsg

readHand :: IO Hand
readHand = (readUserInput "Incorrect hand, try again:")

readFirst :: IO PlayerId
readFirst = (readUserInput "Incorrect player id, try again:")

readMove :: IO Event
readMove = (readUserInput "Incorrect move, try again:")

readPiece :: IO Piece
readPiece = (readUserInput "Incorrect piece, try again:")

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
     -- TODO: should be notified of what was drawn
     EDraw Unknown -> do
             putStrLn "What did I get from the stock?"
             piece <- readPiece
             loop Me newS ((Me,EDraw (Known piece)):evts)
     EPass | head evts == (Opponent,EPass) -> return GRDraw
           | otherwise -> loop Opponent newS ((Me,EPass):evts)
     EMove m | checkWin Me updEvts -> return (GRWin Me)
             | otherwise -> loop Opponent newS updEvts
       where updEvts = ((Me,EMove m):evts)


-- TODO: restore game state from events
-- TODO: parsing errors should not lead to crash

checkWin :: PlayerId -> GameEvents -> Bool
checkWin p evts = numPieces p st == 0
    where numPieces Me       = length . hand
          numPieces Opponent = opponentHand
          st = restoreGameState evts
