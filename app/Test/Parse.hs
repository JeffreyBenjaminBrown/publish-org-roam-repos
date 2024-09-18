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
  , test_bodyParser
  , test_parseFile
  ]

test_linkParser :: Test
test_linkParser = TestCase $ do
  assertBool "" $ parse linkParser "" "[[:id:1][hello]]"
    == Right (OrdinaryText_link "1" "hello")

test_ordinaryTextParser :: Test
test_ordinaryTextParser = TestCase $ do
  assertBool "" $ parse ordinaryTextParser "" "abc"
    == Right (OrdinaryText_text "abc")

test_lineContentParser :: Test
test_lineContentParser = TestCase $ do
  assertBool "" $ parse lineContentParser ""
    "word [[:id:some-id][some link text]] later words"
    == Right [ OrdinaryText_text "word "
             , OrdinaryText_link "some-id" "some link text"
             , OrdinaryText_text " later words" ]

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
  assertBool "" False

test_titleParser :: Test
test_titleParser = TestCase $ do
  assertBool "" $ parse titleParser ""
    "#+title: science with space  "
    == Right (Line_Title " science with space  ")

test_bodyParser :: Test
test_bodyParser = TestCase $ do
  let bodies = sepBy bodyParser newline
      go :: String
         -> Either (ParseErrorBundle String Void) [Line]
         -> Assertion
      go input goal = assertBool "" $
        parse bodies "" input == goal
  go "a\nb"   $ Right [Line_Body "a",Line_Body "b"]
  go "a\nb\n" $ Right [Line_Body "a",Line_Body "b",Line_Body ""]

test_headingParser :: Test
test_headingParser = TestCase $ do
  assertBool "" $ parse headingParser ""
    "** a" == Right (Line_Heading 2 "a")

test_parseFile :: Test
test_parseFile = TestCase $ do
  assertBool "TODO: use data/tiny_test.org" False
