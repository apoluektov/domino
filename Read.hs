module Read where

import Game

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

