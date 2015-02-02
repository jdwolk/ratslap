module Main where

import Test.Tasty

import Ratslap.Test.Card
import Ratslap.Test.Game

tests :: TestTree
tests = testGroup "Tests" [cardSuite, gameSuite]

main :: IO ()
main = defaultMain tests
