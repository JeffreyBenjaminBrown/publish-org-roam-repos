module Tests.Analyze where

import Test.HUnit

import Analyze


allTests :: Test
allTests = TestList
  [ test_is_properties
  , test_id_if_id
  , test_is_title
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
