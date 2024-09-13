module Tests.Analyze.OneLine where

import Test.HUnit

import Analyze.OneLine


allTests :: Test
allTests = TestList
  [ test_headline_to_anchor
  , test_headline_if_headline
  , test_is_properties_line
  , test_id_if_id
  , test_id_if_reference
  , test_is_title
  ]

test_headline_to_anchor :: Test
test_headline_to_anchor = TestCase $ do
  assertBool "" $ headline_to_anchor ",ab." == "ab"
  assertBool "TODO: More headline_to_anchor cases" False

test_headline_if_headline :: Test
test_headline_if_headline = TestCase $ do
  assertBool "" $ headline_if_headline "* a" == Just "a"
  assertBool "" $ headline_if_headline " * a" == Nothing
  assertBool "" $ headline_if_headline "** bab " == Just "bab"

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
