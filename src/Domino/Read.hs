-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanying file MIT-LICENSE)

module Domino.Read
    (
      readHand
    , readFirst
    , readMove
    , readTile
    ) where

import Domino.Game
import Text.ParserCombinators.Parsec
import Data.Char (toUpper)

readHand :: IO Hand
readHand = readUserInput (parser hand) "Incorrect hand, try again:"

readFirst :: IO Player
readFirst = readUserInput (parser player) "Incorrect player id, try again:"

readMove :: IO Event
readMove = readUserInput (parser event) "Incorrect move, try again:"

readTile :: IO Tile
readTile = readUserInput (parser oneTile) "Incorrect tile, try again:"

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

tile :: GenParser Char st Tile
tile = do
  first <- digit
  second <- digit
  return (read [first],read [second])

oneTile :: GenParser Char st Tile
oneTile = do
  p <- tile
  eof
  return p

hand :: GenParser Char st Hand
hand = sepBy tile space

event :: GenParser Char st Event
event = do
  (char 'm' >> move)
  <|> (char 'd' >> draw)
  <|> (char 'p' >> pass)

move :: GenParser Char st Event
move = do
  p <- tile
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

player :: GenParser Char st Player
player = do
  (string "me" >> return Me)
  <|> (string "opponent" >> return Opponent)
