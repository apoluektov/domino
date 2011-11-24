-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Strategy where

import Domino.Game
import Domino.GameState

data Strategy = Strategy (GameState -> (Event, Strategy))

statelessStrategy :: (GameState -> Event) -> Strategy
statelessStrategy f = Strategy g
    where g state = (f state, Strategy g)
