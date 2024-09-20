module Test.Rewrite where

import Test.HUnit

import qualified Data.Map as M
import           Text.Regex

import Parse (parseFile)
import Rewrite
import Types


allTests :: Test
allTests = TestList
  [ test_rewrite_repos
  , test_rewrite_file_pure
  , test_joinLinkText
  ]

test_rewrite_repos :: Test
test_rewrite_repos = TestCase $ do
  assertBool "Did it work?" False

test_rewrite_file_pure :: Test
test_rewrite_file_pure = TestCase $ do
  goalFile <- readFile  "data/tiny_test_online.org"
  e_lines  <- parseFile "data/tiny_test_offline.org"
  let the_lines = either (const []) id e_lines
      repo = Repo
        { repo_local_source       = undefined
        , repo_local_destination  = undefined
        , repo_online_destination = "www.site.com/user/repo" }
      idx = M.fromList
        [ ("1", Node { node_repo = repo
                     , node_file = "this.org"
                     , node_anchor = Nothing } )
        , ("2", Node { node_repo = repo
                     , node_file = "this.org"
                     , node_anchor = Just $ "an-imaginary-headline"
                     } ) ]
      drop_space_pre_colon :: String -> String
      drop_space_pre_colon s = subRegex (mkRegex "^ *:") s ":"
  assertBool "" $ not $ null the_lines
  assertBool "" $ (==)
    (map drop_space_pre_colon $ lines ( rewrite_file_pure
                                        idx the_lines) )
    (map drop_space_pre_colon $ lines goalFile)

test_joinLinkText :: Test
test_joinLinkText = TestCase $ do
  let n = Node
          { node_repo = Repo
              { repo_local_source       = undefined
              , repo_local_destination  = undefined
              , repo_online_destination = "https://github.com/user/repo" }
          , node_file = "filename.org"
          , node_anchor = Nothing }
      m = n { node_anchor = Just "a" }
      l = Link undefined "the displayed text"
  assertBool "" $ joinLinkText n l ==
    "[[https://github.com/user/repo/blob/master/filename.org][the displayed text]]"
  assertBool "" $ joinLinkText m l ==
    "[[https://github.com/user/repo/blob/master/filename.org#a][the displayed text]]"
