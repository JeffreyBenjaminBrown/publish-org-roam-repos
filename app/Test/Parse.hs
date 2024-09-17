module Test.Parse where

import Test.HUnit

-- everything Parse.hs uses
import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Parse
import Types


allTests :: Test
allTests = TestList
  [ test_bodyParser
  ]

test_bodyParser :: Test
test_bodyParser = TestCase $ do
  assertBool "" $ parse (many bodyParser) ""
    "a\nb\n" == Right [Body "a",Body "b"]
