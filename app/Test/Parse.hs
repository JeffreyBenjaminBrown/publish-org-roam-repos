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
  let bodies = sepBy bodyParser newline
      go :: String
         -> Either (ParseErrorBundle String Void) [Line]
         -> Assertion
      go input goal = assertBool "" $
        parse bodies "" input == goal
  go "a\nb"   $ Right [Body "a",Body "b"]
  go "a\nb\n" $ Right [Body "a",Body "b",Body ""]
