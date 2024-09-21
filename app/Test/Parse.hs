module Test.Parse where

import Data.Either (isRight)
import Test.HUnit

import           Text.Megaparsec

import Parse
import Types


allTests :: Test
allTests = TestList
  [ test_linkParser
  , test_ordinaryTextParser
  , test_lineContentParser
  , test_propertiesStartParser
  , test_propertiesEndParser
  , test_uriParser
  , test_titleParser
  , test_headlineParser
  -- , test_bodyParser -- too trivial to test.
  , test_parseFile
  ]

test_linkParser :: Test
test_linkParser = TestCase $ do
  assertBool "" $ parse linkParser "" "[[id:1][hello]]"
    == Right (NormalText_link $ Link  "1" "hello")

test_ordinaryTextParser :: Test
test_ordinaryTextParser = TestCase $ do
  assertBool "" $ parse ordinaryTextParser "" "abc"
    == Right (NormalText_text "abc")

test_lineContentParser :: Test
test_lineContentParser = TestCase $ do
  assertBool "" $ parse lineContentParser ""
    "word [[id:some-id][some link text]] later words"
    == Right [ NormalText_text "word "
             , NormalText_link $ Link  "some-id" "some link text"
             , NormalText_text " later words" ]

test_propertiesStartParser :: Test
test_propertiesStartParser = TestCase $ do
  assertBool "" $ parse propertiesStartParser ""
    ":PROPERTIES:" == Right Line_PropsStart

test_propertiesEndParser :: Test
test_propertiesEndParser = TestCase $ do
  assertBool "" $ parse propertiesEndParser ""
    ":END:" == Right Line_PropsEnd

test_uriParser :: Test
test_uriParser = TestCase $ do
  assertBool "" $ parse uriParser ""
    ":ID:       5x" == Right (Line_URI "5x")

test_titleParser :: Test
test_titleParser = TestCase $ do
  assertBool "" $ parse titleParser ""
    "#+title: science with space  "
    == Right (Line_Title " science with space  ")

test_headlineParser :: Test
test_headlineParser = TestCase $ do
  assertBool "" $ parse headlineParser ""
    "** a" == Right (Line_Headline $
                     Headline 2 [NormalText_text "a"] )

test_parseFile :: Test
test_parseFile = TestCase $ do
  e_the_lines <- parseFile "data/tiny_test_offline.org"
  assertBool "" $ isRight e_the_lines
  let the_lines = case e_the_lines of
        Right x -> x
        Left _  -> undefined -- impossible; see previous assertion
  assertBool "1"  $ the_lines !! 0 == Line_PropsStart
  assertBool "2"  $ the_lines !! 1 == Line_URI "1"
  assertBool "3"  $ the_lines !! 2 == Line_PropsEnd
  assertBool "4"  $ the_lines !! 3 == Line_Title " tiny test file"
  assertBool "5"  $ the_lines !! 4 ==
    ( Line_Headline $ Headline 1
      [ NormalText_text "A link to ",
        NormalText_link $ Link  "1" "this file",
        NormalText_text " and a link to ",
        NormalText_link $ Link "2" "the headline below" ] )
  assertBool "6"  $ the_lines !! 5 ==
    ( Line_Headline $ Headline 2
      [NormalText_text "an imaginary headline"] )
  assertBool "7"  $ the_lines !! 6 == Line_PropsStart
  assertBool "8"  $ the_lines !! 7 == Line_URI "2"
  assertBool "9"  $ the_lines !! 8 == Line_PropsEnd
  assertBool "10" $ the_lines !! 9 ==
    Line_Body [NormalText_text "   With some text."]
  assertBool "11" $ the_lines !! 10 == Line_Body []
