module Test.Parse where

import Test.HUnit

-- everything Parse.hs uses
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Parse
import Types


allTests :: Test
allTests = TestList
  [ test_linkParser
  , test_ordinaryTextParser
  , test_lineContentParser
  , test_propertiesStartParser
  , test_propertiesEndParser
  , test_idParser
  , test_titleParser
  , test_headingParser
  -- , test_bodyParser -- too trivial to test.
  , test_parseFile
  ]

test_linkParser :: Test
test_linkParser = TestCase $ do
  assertBool "" $ parse linkParser "" "[[:id:1][hello]]"
    == Right (NormalText_link "1" "hello")

test_ordinaryTextParser :: Test
test_ordinaryTextParser = TestCase $ do
  assertBool "" $ parse ordinaryTextParser "" "abc"
    == Right (NormalText_text "abc")

test_lineContentParser :: Test
test_lineContentParser = TestCase $ do
  assertBool "" $ parse lineContentParser ""
    "word [[:id:some-id][some link text]] later words"
    == Right [ NormalText_text "word "
             , NormalText_link "some-id" "some link text"
             , NormalText_text " later words" ]

test_propertiesStartParser :: Test
test_propertiesStartParser = TestCase $ do
  assertBool "" $ parse propertiesStartParser ""
    ":PROPERTIES:" == Right Line_PropsStart

test_propertiesEndParser :: Test
test_propertiesEndParser = TestCase $ do
  assertBool "" $ parse propertiesEndParser ""
    ":END:" == Right Line_PropsEnd

test_idParser :: Test
test_idParser = TestCase $ do
  assertBool "" $ parse idParser ""
    ":ID:       5x" == Right (Line_Id "5x")

test_titleParser :: Test
test_titleParser = TestCase $ do
  assertBool "" $ parse titleParser ""
    "#+title: science with space  "
    == Right (Line_Title " science with space  ")

test_headingParser :: Test
test_headingParser = TestCase $ do
  assertBool "" $ parse headingParser ""
    "** a" == Right (Line_Heading 2 [NormalText_text "a"])

test_parseFile :: Test
test_parseFile = TestCase $ do
  the_lines <- parseFile "data/tiny_test.org"
  assertBool "TODO: Add lines to goal below." $ the_lines ==
    Right [ (1, Line_PropsStart)
          , (2, Line_Id "1")
          , (3, Line_PropsEnd)
          , (4, Line_Title " tiny test file") ]
