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
  assertBool "" $ parse (sepBy bodyParser newline)
    "" "a\nb" == Right [Body "a",Body "b"]
