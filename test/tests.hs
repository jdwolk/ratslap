module Main where

import Test.Tasty

import Ratslap.Test.Card
import Ratslap.Test.Game
import Ratslap.Test.Deck

tests :: TestTree
tests = testGroup "Tests" [cardSuite, gameSuite, deckSuite]

main :: IO ()
main = defaultMain tests
