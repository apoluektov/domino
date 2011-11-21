-- Copyright (c) 2011 Alexander Poluektov (alexander.poluektov@gmail.com)
--
-- Use, modification and distribution are subject to the MIT license
-- (See accompanyung file MIT-LICENSE)

module Domino.Strategy where

import Domino.Game

data Strategy = Strategy (GameEvents -> (Event, Strategy))

statelessStrategy :: (GameEvents -> Event) -> Strategy
statelessStrategy f = Strategy g
    where g evts = (f evts, Strategy g)
