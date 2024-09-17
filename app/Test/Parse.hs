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
  [
    test_titleParser
  , test_headingParser
  , test_bodyParser
  ]

test_titleParser :: Test
test_titleParser = TestCase $ do
  assertBool "" $ parse titleParser ""
    "#+title: science with space  "
    == Right (Title " science with space  ")

test_bodyParser :: Test
test_bodyParser = TestCase $ do
  let bodies = sepBy bodyParser newline
      go :: String
         -> Either (ParseErrorBundle String Void) [Line]
         -> Assertion
      go input goal = assertBool "" $
        parse bodies "" input == goal
  go "a\nb"   $ Right [Body "a",Body "b"]
  go "a\nb\n" $ Right [Body "a",Body "b",Body ""]

test_headingParser :: Test
test_headingParser = TestCase $ do
  assertBool "" $ parse headingParser ""
    "** a" == Right (Heading 2 "a")
