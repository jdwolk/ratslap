{-Test sample data for instances of Arbitrary:-}

:m +Test.Tasty.QuickCheck
:m +Ratslap.Test.Game
let a = arbitrary :: Test.Tasty.QuickCheck.Gen ShuffleType
sample a

let b = arbitrary :: Test.Tasty.QuickCheck.Gen StackOrder
sample b

deckFrom $ makeStackOrder $ TopTwoSame Three

deckFrom $ makeStackOrder $ TopThreeSandwich Jack

