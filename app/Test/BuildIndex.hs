module Test.BuildIndex where

import Data.Either (isRight)
import Test.HUnit

import BuildIndex
import Parse (parseFile)
import Types


allTests :: Test
allTests = TestList
  [ test_indexFile
  ]

test_indexFile :: Test
test_indexFile = TestCase $ do
  e_lines <- parseFile "data/tiny_test_offline.org"
  assertBool "" $ isRight e_lines
  let the_lines = case e_lines of
        Right x -> x
        Left _ -> undefined -- impossible, as asserted above
      r = Repo "nickname" "local" "online"
  assertBool "" $ indexFile r "filepath" the_lines
    == [ Node { node_uri = "1",
                node_repo = r,
                node_file = "filepath",
                node_anchor = Nothing }
       , Node { node_uri = "2",
                node_repo = r,
                node_file = "filepath",
                node_anchor = Just "an-imaginary-headline" } ]

  e_lines_2 <- parseFile "data/duplicate_anchors.org"
  assertBool "" $ isRight e_lines_2
  let the_lines_2 = case e_lines_2 of
        Right x -> x
        Left _ -> undefined -- impossible, as asserted above
  assertBool "" $ indexFile r "filepath" the_lines_2
    == [ Node { node_uri = "0",
                node_repo = r,
                node_file = "filepath",
                node_anchor = Nothing }
       , Node { node_uri = "1",
                node_repo = r,
                node_file = "filepath",
                node_anchor = Just "a-duplicated-headline" }
       , Node { node_uri = "2",
                node_repo = r,
                node_file = "filepath",
                node_anchor = Just "a-duplicated-headline-1" } ]
