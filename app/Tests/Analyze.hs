module Tests.Analyze where

import Test.HUnit

import Analyze


allTests :: Test
allTests = TestList
  [ test_is_properties
  ]

test_is_properties :: Test
test_is_properties = TestCase $ do
  assertBool "" $       is_properties ":PROPERTIES:"
  assertBool "" $       is_properties "    :PROPERTIES:    "
  assertBool "" $ not $ is_properties "hey :PROPERTIES:    "
  assertBool "" $ not $ is_properties "    :PROPERTIES: hey"
  assertBool "" $ not $ is_properties ":PATTIES:"
