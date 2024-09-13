module Tests.Analyze where

import Test.HUnit

import Analyze


allTests :: Test
allTests = TestList
  [ test_countTitleLine
  , test_linesOfIds
  ]

test_countTitleLine :: Test
test_countTitleLine = TestCase $ do
  assertBool "" $
    countTitleLine [ "whatever",
                     "line 2 isn't it either",
                     "#+title: science" ]
    == Just 3
  assertBool "" $
    countTitleLine [ "there isn't one",
                     "in this file" ]
    == Nothing

test_linesOfIds :: Test
test_linesOfIds = TestCase $ do
  let file =
        [ ":PROPERTIES:"
        , ":ID:       111"
        , ":END:"
        , "#+title: science"
        , "* [[id:c35ab968-7056-40fa-8816-ea16d5c88f6d][a list of sciences]]"
        , "* some text, whatever"
        , "** lalala"
        , "* an apparently important headline"
        , "  :PROPERTIES:"
        , "  :ID:       222" -- the 10th line (1-indexed)
        , "  :END:"
        , "* even more text"
        , "** lalala" ]
      titleLine :: Int = maybe (-1) id $ countTitleLine file
  assertBool "" $ titleLine == 4
  assertBool "" $ linesOfIds titleLine file
    == [ ("111", Nothing),
         ("222", Just 10) ]
