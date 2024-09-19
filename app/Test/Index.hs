module Test.Index where

import Data.Either (isRight)
import Test.HUnit

import Index
import Parse (parseFile)
import Types


allTests :: Test
allTests = TestList
  [ test_indexFile
  ]

test_indexFile :: Test
test_indexFile = TestCase $ do
  e_the_lines <- parseFile "data/tiny_test.org"
  assertBool "" $ isRight e_the_lines
  let the_lines = case e_the_lines of
        Right x -> x
        Left _ -> undefined -- impossible, as asserted above
      r = Repo "nickname" "local" "online"
  assertBool "" $ indexFile r "filepath" the_lines
    == [Node {node_uri = "2",
              node_repo = r,
              node_file = "filepath",
              node_line = Just 6},
        Node {node_uri = "1",
              node_repo = r,
              node_file = "filepath",
              node_line = Nothing}]
