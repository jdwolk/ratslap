module Ratslap.Test.Game where

import           Ratslap.Game          (slapValid)
import           Ratslap.Card          (CardVal(..))
import           Ratslap.Deck          (deckFrom, makeStackOrder, ShuffleType(..))
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty, Arbitrary(..), elements)

instance Arbitrary CardVal where
  arbitrary = elements [Two .. Ace]

gameSuite :: TestTree
gameSuite = testGroup "Game Tests" [
    testProperty
      "slapValid is true if the top two cards are the same value"
      prop_slapValidIfTopTwoCardsHaveSameValue,
    testProperty
      "slapValid is true if the top three cards form a sandwich"
      prop_slapValidIfTopThreeCardsAreSandwich
  ]

prop_slapValidIfTopTwoCardsHaveSameValue :: CardVal -> Bool
prop_slapValidIfTopTwoCardsHaveSameValue cv =
  slapValid testDeck
    where testDeck = deckFrom $ makeStackOrder $ TopTwoSame cv


prop_slapValidIfTopThreeCardsAreSandwich :: CardVal -> Bool
prop_slapValidIfTopThreeCardsAreSandwich cv =
  slapValid testDeck
    where testDeck = deckFrom $ makeStackOrder $ TopThreeSandwich cv

