module Ratslap.Deck (
    CardRestriction(..)
  , ShuffleType(..)
  , StackOrder(..)
  , Deck
  , deck
  , deckFrom
  , makeStackOrder
  ) where

import Ratslap.Card (Card(..), CardVal(..), Suit(..))

type Deck = [Card]

data CardRestriction = WithFixedVal CardVal |
                       WithAnyButVal CardVal |
                       WithRandVal deriving (Show)

data ShuffleType = TopTwoSame CardVal |
                   TopThreeSandwich CardVal deriving (Show)

newtype StackOrder = StackOrder [CardRestriction]
                     deriving (Show)

deck :: Deck
deck = [Card v s | v <- [Two .. Ace], s <- [Clubs .. Spades] ]

makeStackOrder :: ShuffleType -> StackOrder
makeStackOrder (TopTwoSame val)       = StackOrder [ WithFixedVal val
                                                   , WithFixedVal val
                                                   , WithAnyButVal val ]
makeStackOrder (TopThreeSandwich val) = StackOrder [ WithFixedVal val
                                                   , WithAnyButVal val
                                                   , WithFixedVal val]

deckFrom :: StackOrder -> Deck
deckFrom (StackOrder rs) = deckFrom' rs [] deck

deckFrom' :: [CardRestriction] -> [Card] -> [Card] -> Deck
deckFrom' []                           toKeep rest = toKeep ++ rest
deckFrom' (WithRandVal : moreRs)       toKeep rest =
  deckFrom'' (const True) moreRs toKeep rest -- not _really_ random...
deckFrom' (WithAnyButVal val : moreRs) toKeep rest =
  deckFrom'' (\c -> cardVal c /= val) moreRs toKeep rest
deckFrom' (WithFixedVal val : moreRs)  toKeep rest =
  deckFrom'' (\c -> cardVal c == val) moreRs toKeep rest

deckFrom'' :: (Card -> Bool) -> [CardRestriction] -> [Card] -> [Card] -> Deck
deckFrom'' predFn moreRs toKeep rest =
  deckFrom' moreRs (toKeep ++ [nextCard]) otherCards
    where
      (nonMatches, matches) = break predFn rest
      nextCard              = head matches
      otherCards            = tail matches ++ nonMatches

