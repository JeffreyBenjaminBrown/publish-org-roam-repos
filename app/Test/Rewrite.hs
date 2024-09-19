module Test.Rewrite where

import Test.HUnit

import Rewrite
import Types


allTests :: Test
allTests = TestList
  [ test_joinLinkText
  ]

test_joinLinkText :: Test
test_joinLinkText = TestCase $ do
  let r = Repo { repo_name = undefined
               , repo_local_path = undefined
               , repo_online_path = "https://github.com/user/repo" }
      p = "filename.org"
      l = Link undefined "the displayed text"
  assertBool "" $ joinLinkText r p l ==
    "[[https://github.com/user/repo/blob/master/filename.org][the displayed text]]"
