module Ratslap.Test.Card where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Ratslap.Card (CardVal(..), deck, cardVal)

instance Arbitrary CardVal where
  arbitrary = elements [Two .. Ace]

cardSuite :: TestTree
cardSuite = testGroup "Card Tests" [
  QC.testProperty "52 cards in a deck" prop_deckLength,
  QC.testProperty "4 cards of each value in a deck" prop_cardValInDeck
  ]

prop_deckLength :: Bool
prop_deckLength = length deck == 52

prop_cardValInDeck :: CardVal -> Bool
prop_cardValInDeck v = length cardsWithVal == 4
  where cardsWithVal = filter (\ c -> cardVal c == v) deck

