module Ratslap.Game (
  slapValid
) where

import Ratslap.Card

slapValid :: [Card] -> Bool
slapValid (x:y:_)   = cardVal x == cardVal y
slapValid (x:_:z:_) = cardVal x == cardVal z
slapValid _         = False

