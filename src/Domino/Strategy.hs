-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

{-# LANGUAGE GADTs #-}

module Domino.Strategy where

import Domino.Game

data Strategy where
    Strategy :: (a -> (Player,Event) -> Strategy)
             -> (a -> Event)
             -> a
             -> Strategy

notify :: Strategy -> (Player,Event) -> Strategy
notify (Strategy f _ st) e = f st e

next :: Strategy -> Event
next (Strategy _ f st) = f st
