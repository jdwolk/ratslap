module GameHspec where

import           Test.Hspec
import           Test.QuickCheck
import           Ratslap.Game          (slapValid)
import           Ratslap.Card          (CardVal(..))
import           Ratslap.Deck          (deckFrom, makeStackOrder, ShuffleType(..))

main :: IO ()
main = hspec $ do
  describe "slapValid" $ do
    it "is true is the top two cards are the same value" $
      property $ \cv -> slapValid testDeck
        where testDeck = deckFrom $ makeStackOrder $ TopTwoSame cv

    it "is true if the top three cards form a sandwich" $
      property $ \cv -> slapValid testDeck
        where testDeck = deckFrom $ makeStackOrder $ TopThreeSandwich cv

