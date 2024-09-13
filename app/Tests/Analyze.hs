module Tests.Analyze where

import Test.HUnit

import Analyze


allTests :: Test
allTests = TestList
  [ test_headline_to_anchor
  , test_is_properties_line
  , test_id_if_id
  , test_id_if_reference
  , test_is_title
  , test_countTitleLine
  , test_header_if_header
  ]

test_header_if_header :: Test
test_header_if_header = TestCase $ do
  assertBool "" $ header_if_header "* a" == Just "a"
  assertBool "" $ header_if_header " * a" == Nothing
  assertBool "" $ header_if_header "** bab " == Just "bab"

test_headline_to_anchor :: Test
test_headline_to_anchor = TestCase $ do
  assertBool "" $ headline_to_anchor ",ab." == "ab"
  assertBool "TODO: More headline_to_anchor cases" False

test_is_properties_line :: Test
test_is_properties_line = TestCase $ do
  assertBool "" $       is_properties_line ":PROPERTIES:"
  assertBool "" $       is_properties_line "    :PROPERTIES:    "
  assertBool "" $ not $ is_properties_line "hey :PROPERTIES:    "
  assertBool "" $ not $ is_properties_line "    :PROPERTIES: hey"
  assertBool "" $ not $ is_properties_line ":PATTIES:"

test_id_if_id :: Test
test_id_if_id = TestCase $ do
  assertBool "" $ id_if_id "  :ID: 123  " == Just "123"
  assertBool "" $ id_if_id "  123  :ID:  " == Nothing

test_id_if_reference :: Test
test_id_if_reference = TestCase $ do
  assertBool "" $ id_if_reference "  :ID: 123  "
    == Nothing
  assertBool "" $ id_if_reference "[[id:a1][something]]"
    == Just "a1"

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
      titleLine :: Int = maybe (-1) id $ countTitleLine file
  assertBool "" $ titleLine == 4
  assertBool "" $ linesOfIds titleLine file
    == [ ("111", Nothing),
         ("222", Just 10) ]
