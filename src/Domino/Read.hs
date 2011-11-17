-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Read
    (
      readHand
    , readFirst
    , readMove
    , readPiece
    ) where

import Domino.Game
import Text.ParserCombinators.Parsec
import Data.Char (toUpper)

readHand :: IO Hand
readHand = readUserInput (parser hand) "Incorrect hand, try again:"

readFirst :: IO PlayerId
readFirst = readUserInput (parser playerId) "Incorrect player id, try again:"

readMove :: IO Event
readMove = readUserInput (parser event) "Incorrect move, try again:"

readPiece :: IO Piece
readPiece = readUserInput (parser onePiece) "Incorrect piece, try again:"

readUserInput :: (String -> Either ParseError a) -> String -> IO a
readUserInput parse retryMsg = do
  s <- getLine
  let r = parse s
  case r of
    Right i -> return i
    Left  m -> do
           putStrLn $ retryMsg
           readUserInput parse retryMsg

parser :: GenParser Char () a -> String -> Either ParseError a
parser p = parse p "input"

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
