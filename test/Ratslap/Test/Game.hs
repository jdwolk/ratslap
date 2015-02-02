module Ratslap.Test.Game (
  CardRestriction(..),
  ShuffleType(..),
  StackOrder(..),
  deckFrom, makeStackOrder, gameSuite
) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Ratslap.Card (Card(..), CardVal(..), Deck, deck)
import Ratslap.Game (slapValid)
import Control.Monad
import Data.List (break)

--------------------------------------------------
-- TODO: Should probably pull this out since its useful
-- outside of just the testing context
-------------------------------------------------

data CardRestriction = WithFixedVal CardVal |
                       WithAnyButVal CardVal |
                       WithRandVal deriving (Show)

data ShuffleType = TopTwoSame CardVal |
                   TopThreeSandwich CardVal deriving (Show)

newtype StackOrder = StackOrder [CardRestriction]
                     deriving (Show)


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
  deckFrom' moreRs (head rest:toKeep) $ tail rest
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

-------------------

instance Arbitrary CardVal where
  arbitrary = elements [ Two .. Ace ]

instance Arbitrary ShuffleType where
  arbitrary = oneof [ liftM TopTwoSame arbitrary,
                      liftM TopThreeSandwich arbitrary
                    ]

instance Arbitrary StackOrder where
  arbitrary = liftM makeStackOrder arbitrary

gameSuite :: TestTree
gameSuite = testGroup "Game Tests" [
    QC.testProperty
      "TopTwoSame ShuffleType always has the top two cards with same card value"
      prop_topTwoSameShuffleTypeIsCorrect,
    QC.testProperty
      "TopThreeSandwich ShuffleType always has the first and third card with same value"
      prop_topThreeSandwichShuffleTypeIsCorrect
  ]


prop_topTwoSameShuffleTypeIsCorrect :: CardVal -> Bool
prop_topTwoSameShuffleTypeIsCorrect cv =
  all (\c -> cardVal c == cv) [firstCard, secondCard]
    where (firstCard:secondCard:_) = deckFrom $ makeStackOrder $ TopTwoSame cv


prop_topThreeSandwichShuffleTypeIsCorrect :: CardVal -> Bool
prop_topThreeSandwichShuffleTypeIsCorrect cv =
  all (\c -> cardVal c == cv) [firstCard, thirdCard]
    where (firstCard:_:thirdCard:_) = deckFrom $ makeStackOrder $ TopThreeSandwich cv

