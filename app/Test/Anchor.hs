-- | For turning org-headlines into Github URL anchors,
-- which permit linking to a specific line in a file.

module Test.Anchor where

import Test.HUnit

import Anchor
import Types


allTests :: Test
allTests = TestList
  [ test_headline_to_visibleText
  , test_mangleAnchorPunctuation
  , test_replaceDoubleDash
  ]

test_replaceDoubleDash :: Test
test_replaceDoubleDash = TestCase $ do
  assertBool "" $ replaceDoubleDash "--" == ""
  assertBool "" $ replaceDoubleDash "-- --" == " "
  assertBool "" $ replaceDoubleDash "-- -- --" == "  "
  assertBool "" $ replaceDoubleDash "----" == "----"
  assertBool "" $ replaceDoubleDash "--x--x-x--" == "xx-x"
  assertBool "" $ replaceDoubleDash "-x-x--x-" == "-x-xx-"

test_mangleAnchorPunctuation :: Test
test_mangleAnchorPunctuation = TestCase $ do
  assertBool "" $ mangleAnchorPunctuation "!-!-!" == "--"
  assertBool "" $ mangleAnchorPunctuation "--a-!-b-- " == "a--b-"

test_headline_to_visibleText :: Test
test_headline_to_visibleText = TestCase $ do
  assertBool "" $ "hello link name goodbye"
    == ( headline_to_visibleText
         $ Headline undefined
         [ NormalText_text " hello "
         , NormalText_link $ Link undefined "link name"
         , NormalText_text " goodbye " ] )
