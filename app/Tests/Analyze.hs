module Tests.Analyze where

import Test.HUnit

import Analyze


allTests :: Test
allTests = TestList
  [ test_is_properties
  , test_id_if_id
  , test_is_title
  , test_countTitleLine
  ]

test_is_properties :: Test
test_is_properties = TestCase $ do
  assertBool "" $       is_properties ":PROPERTIES:"
  assertBool "" $       is_properties "    :PROPERTIES:    "
  assertBool "" $ not $ is_properties "hey :PROPERTIES:    "
  assertBool "" $ not $ is_properties "    :PROPERTIES: hey"
  assertBool "" $ not $ is_properties ":PATTIES:"

test_id_if_id :: Test
test_id_if_id = TestCase $ do
  assertBool "" $ id_if_id "  :ID: 123  " == Just "123"
  assertBool "" $ id_if_id "  123  :ID:  " == Nothing

test_is_title :: Test
test_is_title = TestCase $ do
  assertBool "" $       is_title "#+title: science"
  assertBool "" $       is_title "  #+title:    science is fun  "
  assertBool "" $ not $ is_title "#+  title: science" -- the space after the `+` disqualifies it

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
        , "* an apparently important header"
        , "  :PROPERTIES:"
        , "  :ID:       222" -- the 10th line (1-indexed)
        , "  :END:"
        , "* even more text"
        , "** lalala" ]
      Just titleLine = countTitleLine file
  assertBool "" $ linesOfIds titleLine file
    == [ ("111", Nothing),
         ("222", Just 10) ]
