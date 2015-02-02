module Ratslap.Test.Game (
  CardRestriction(..),
  ShuffleType(..),
  StackOrder(..),
  deckFrom, makeStackOrder
) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Ratslap.Card (Card(..), CardVal(..), Deck, deck)
import Ratslap.Game (slapValid)
import Control.Monad
import Data.List (break)

data CardRestriction = WithFixedVal CardVal | WithRandVal deriving (Show)

data ShuffleType = TopTwoSame CardVal |
                   TopThreeSandwich CardVal |
                   Shuffled deriving (Show)

newtype StackOrder = StackOrder [CardRestriction]
                     deriving (Show)

instance Arbitrary CardVal where
  arbitrary = elements [ Two .. Ace ]

instance Arbitrary ShuffleType where
  arbitrary = oneof [ liftM TopTwoSame arbitrary,
                      liftM TopThreeSandwich arbitrary,
                      elements [Shuffled]
                    ]

instance Arbitrary StackOrder where
  arbitrary = do
    st <- arbitrary
    return (makeStackOrder st)

makeStackOrder :: ShuffleType -> StackOrder
makeStackOrder (TopTwoSame val)       = StackOrder [ WithFixedVal val
                                                   , WithFixedVal val]
makeStackOrder (TopThreeSandwich val) = StackOrder [ WithFixedVal val
                                                   , WithRandVal
                                                   , WithFixedVal val]
makeStackOrder Shuffled               = StackOrder []

deckFrom :: StackOrder -> Deck
deckFrom (StackOrder rs) = deckFrom' rs [] deck

deckFrom' :: [CardRestriction] -> [Card] -> [Card] -> Deck
deckFrom' []                           toKeep rest = toKeep ++ rest
deckFrom' (WithRandVal : moreRs)       toKeep rest =
  deckFrom' moreRs (head rest:toKeep) $ tail rest
deckFrom' (WithFixedVal val : moreRs)  toKeep rest =
  deckFrom' moreRs (nextCard:toKeep) otherCards
    where
      (nonMatches, matches) = break (\c -> cardVal c == val) rest
      nextCard              = head matches
      otherCards            = tail matches ++ nonMatches

-- take 2 cards from the deck suchthat their CardVals are the same


