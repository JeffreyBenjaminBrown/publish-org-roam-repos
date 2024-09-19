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
  let n = Node
          { node_uri = undefined
          , node_repo = Repo {
              repo_name = undefined
              , repo_local_path = undefined
              , repo_online_path = "https://github.com/user/repo" }
          , node_file = "filename.org"
          , node_headline = Nothing }
      l = Link undefined "the displayed text"
  assertBool "" $ joinLinkText n l ==
    "[[https://github.com/user/repo/blob/master/filename.org][the displayed text]]"
  assertBool "TODO: One with a headline." False
