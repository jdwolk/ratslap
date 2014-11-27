module Ratslap.Game (
  topTwoCards
) where

import Ratslap.Card

type CardWindow = [Card]

topTwoCards :: CardWindow -> (Maybe Card, Maybe Card)
topTwoCards [] = (Nothing, Nothing)
topTwoCards [c] = (Just c, Nothing)
topTwoCards (x:y:_) = (Just x, Just y)

