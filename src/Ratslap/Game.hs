module Ratslap.Game (
  slapValid
) where

import Ratslap.Card

slapValid :: Deck -> Bool
slapValid (x:y:z:_) = cardVal x == cardVal y ||
                      cardVal x == cardVal z
slapValid _         = False

