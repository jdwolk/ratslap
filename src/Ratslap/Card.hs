module Ratslap.Card (
  Card(..),
  Suit(..),
  CardVal(..),
  deck
) where

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Read, Enum)

instance Show Suit where
  show Clubs    = "\x2663"
  show Diamonds = "\x2666"
  show Hearts   = "\x2665"
  show Spades   = "\x2660"


data CardVal = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
               Jack | Queen | King | Ace
               deriving (Eq, Read, Enum, Bounded)

instance Show CardVal where
  show Two    = show 2
  show Three  = show 3
  show Four   = show 4
  show Five   = show 5
  show Six    = show 6
  show Seven  = show 7
  show Eight  = show 8
  show Nine   = show 9
  show Ten    = show 10
  show Jack   = "J"
  show Queen  = "Q"
  show King   = "K"
  show Ace    = "A"


data Card = Card CardVal Suit deriving (Eq, Read)

instance Show Card where
  show (Card v s) = "[" ++ show v ++ show s ++ "]"

deck :: [Card]
deck = [Card v s | v <- [Two .. Ace], s <- [Clubs .. Spades] ]

