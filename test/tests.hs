module Main where

import Test.Tasty

import Ratslap.Card.Test

tests :: TestTree
tests = testGroup "Tests" [cardSuite]

main :: IO ()
main = defaultMain tests
