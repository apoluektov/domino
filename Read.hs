module Read
    (
      readHand
    , readFirst
    , readMove
    , readPiece
    ) where

import Game
import Text.ParserCombinators.Parsec
import Data.Char (toUpper)

readUserInput :: (String -> Either ParseError a) -> String -> IO a
readUserInput parse retryMsg = do
  s <- getLine
  let r = parse s
  case r of
    Right i -> return i
    Left  m -> do
           putStrLn $ retryMsg
           readUserInput parse retryMsg

readHand :: IO Hand
readHand = (readUserInput parseHand "Incorrect hand, try again:")

readFirst :: IO PlayerId
readFirst = (readUserInput parsePlayerId "Incorrect player id, try again:")

readMove :: IO Event
readMove = (readUserInput parseEvent "Incorrect move, try again:")

readPiece :: IO Piece
readPiece = (readUserInput parsePiece "Incorrect piece, try again:")


piece :: GenParser Char st Piece
piece = do
  first <- digit
  second <- digit
  return (read [first],read [second])

onePiece :: GenParser Char st Piece
onePiece = do
  p <- piece
  eof
  return p

hand :: GenParser Char st Hand
hand = sepBy piece space

event :: GenParser Char st Event
event = do
  (char 'm' >> move)
  <|> (char 'd' >> draw)
  <|> (char 'p' >> pass)

move :: GenParser Char st Event
move = do
  p <- piece
  d <- oneOf "lLrR"
  eof
  return $ EMove (Move p (read [toUpper d]))

draw :: GenParser Char st Event
draw = do
  eof
  return $ EDraw Unknown

pass :: GenParser Char st Event
pass = do
  eof
  return $ EPass

playerId :: GenParser Char st PlayerId
playerId = do
  (string "me" >> return Me)
  <|> (string "opponent" >> return Opponent)

parsePiece = parse onePiece "input"
parseHand = parse hand "input"
parseEvent = parse event "input"
parsePlayerId = parse playerId "input"
