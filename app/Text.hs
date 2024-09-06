module Text where

import qualified Data.Text as T
import           Text.Regex


strip :: String -> String
strip = T.unpack . T.strip . T.pack

lines_demo :: [String]
lines_demo = lines "   a   \nb\n"

-- ^ The symbols ^ and $ work as hoped.
match_demo :: [Maybe [String]]
match_demo = map (matchRegex (mkRegex "^hello$"))
  [" hello",
   "hello ",
   "hello" ]

-- ^ The list is of subexpression matches, i.e. capture groups,
-- which are denoted by parentheses.
match_capture_demo :: Maybe [String]
match_capture_demo = matchRegex
                     (mkRegex "([a-z]+)([0-9]+)([a-z]+)")
                     "aaa123zzz"
