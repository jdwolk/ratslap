module Ratslap.Test.Deck where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), elements, oneof)
import Ratslap.Card (CardVal(..), cardVal)
import Ratslap.Deck (
    Deck
  , ShuffleType(..)
  , StackOrder(..)
  , deck
  , deckFrom
  , makeStackOrder
  )
import Control.Monad

instance Arbitrary CardVal where
  arbitrary = elements [Two .. Ace]

instance Arbitrary ShuffleType where
  arbitrary = oneof [ liftM TopTwoSame arbitrary,
                      liftM TopThreeSandwich arbitrary
                    ]

instance Arbitrary StackOrder where
  arbitrary = liftM makeStackOrder arbitrary

deckSuite :: TestTree
deckSuite = testGroup "Deck Tests" [
  testProperty "52 cards in a deck" prop_deckLength,
  testProperty "4 cards of each value in a deck" prop_cardValInDeck,
  testProperty
    "TopTwoSame ShuffleType always has the top two cards with same card value"
    prop_topTwoSameShuffleTypeIsCorrect,
  testProperty
    "TopThreeSandwich ShuffleType always has the first and third card with same value"
    prop_topThreeSandwichShuffleTypeIsCorrect
  ]


prop_deckLength :: Bool
prop_deckLength = length deck == 52


prop_cardValInDeck :: CardVal -> Bool
prop_cardValInDeck v = length cardsWithVal == 4
  where cardsWithVal = filter (\ c -> cardVal c == v) deck


prop_topTwoSameShuffleTypeIsCorrect :: CardVal -> Bool
prop_topTwoSameShuffleTypeIsCorrect cv =
  all (\c -> cardVal c == cv) [firstCard, secondCard]
    where (firstCard:secondCard:_) = deckFrom $ makeStackOrder $ TopTwoSame cv


prop_topThreeSandwichShuffleTypeIsCorrect :: CardVal -> Bool
prop_topThreeSandwichShuffleTypeIsCorrect cv =
  all (\c -> cardVal c == cv) [firstCard, thirdCard]
    where (firstCard:_:thirdCard:_) = deckFrom $ makeStackOrder $ TopThreeSandwich cv
