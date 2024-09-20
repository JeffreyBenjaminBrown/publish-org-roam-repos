module Test.BuildIndex where

import           Data.Either (isRight)
import qualified Data.Map as M
import           Test.HUnit

import BuildIndex
import Config (this_repo)
import Parse (parseFile)
import Types


allTests :: Test
allTests = TestList
  [ test_addFileToIndex
  , test_indexFile
  ]

test_addFileToIndex :: Test
test_addFileToIndex = TestCase $ do
  let repo = Repo this_repo "local_dest" "online_dest"
      path = "data/tiny_test_offline.org"
  result <- addFileToIndex repo path ([], mempty)
  assertBool "" $ result ==
    ( []
    , M.fromList
      [ ("1", Node { node_repo = repo
                   , node_file = path
                   , node_anchor = Nothing } )
      , ("2", Node { node_repo = repo
                   , node_file = path
                   , node_anchor = Just "an-imaginary-headline"
                   } ) ] )

test_indexFile :: Test
test_indexFile = TestCase $ do
  e_lines <- parseFile "data/tiny_test_offline.org"
  assertBool "" $ isRight e_lines
  let the_lines = case e_lines of
        Right x -> x
        Left _ -> undefined -- impossible, as asserted above
      r = Repo "local_source" "local_dest" "online_dest"
  assertBool "" $ indexFile r "filepath" the_lines
    == [ ("1", Node { node_repo = r,
                      node_file = "filepath",
                      node_anchor = Nothing } )
       , ("2", Node { node_repo = r,
                      node_file = "filepath",
                      node_anchor = Just "an-imaginary-headline"
                    } ) ]

  e_lines_2 <- parseFile "data/duplicate_anchors.org"
  assertBool "" $ isRight e_lines_2
  let the_lines_2 = case e_lines_2 of
        Right x -> x
        Left _ -> undefined -- impossible, as asserted above
  assertBool "" $ indexFile r "filepath" the_lines_2
    == [ ("0", Node { node_repo = r,
                      node_file = "filepath",
                      node_anchor = Nothing } )
       , ("1", Node { node_repo = r,
                      node_file = "filepath",
                      node_anchor = Just "a-duplicated-headline" } )
       , ("2", Node { node_repo = r,
                      node_file = "filepath",
                      node_anchor = Just "a-duplicated-headline-1"
                    } ) ]
