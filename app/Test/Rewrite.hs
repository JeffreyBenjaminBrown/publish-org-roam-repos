module Test.Rewrite where

import Test.HUnit

import Rewrite
import Types


allTests :: Test
allTests = TestList
  [ test_replaceDoubleDash
  , test_mangleAnchorPunctuation
  , test_joinLinkText
  ]

test_mangleAnchorPunctuation :: Test
test_mangleAnchorPunctuation = TestCase $ do
  assertBool "" $ mangleAnchorPunctuation "!-!-!" == "--"
  assertBool "" $ mangleAnchorPunctuation "--a-!-b-- " == "a--b-"

test_replaceDoubleDash :: Test
test_replaceDoubleDash = TestCase $ do
  assertBool "" $ replaceDoubleDash "--" == ""
  assertBool "" $ replaceDoubleDash "-- --" == " "
  assertBool "" $ replaceDoubleDash "-- -- --" == "  "
  assertBool "" $ replaceDoubleDash "----" == "----"
  assertBool "" $ replaceDoubleDash "--x--x-x--" == "xx-x"
  assertBool "" $ replaceDoubleDash "-x-x--x-" == "-x-xx-"

test_joinLinkText :: Test
test_joinLinkText = TestCase $ do
  let r = Repo { repo_name = undefined
               , repo_local_path = undefined
               , repo_online_path = "https://github.com/user/repo" }
      p = "filename.org"
      l = Link undefined "the displayed text"
  assertBool "" $ joinLinkText r p l ==
    "[[https://github.com/user/repo/blob/master/filename.org][the displayed text]]"
