-- | Simple parsers that look at a single line of a file,
-- or maybe even less.

module Analyze.OneLine where

import           Data.Maybe
import           Text.Regex

import Util (strip)


headline_if_headline :: String -> Maybe String
headline_if_headline s = strip . head <$> matchRegex regex s
  where regex = mkRegex "^\\*+ (.*)$"

is_properties_line :: String -> Bool
is_properties_line =
  isJust . matchRegex (mkRegex "^ *:PROPERTIES: *$")

-- | This identifies the passage wherein an ID identifies a header.
-- That is, these IDs are not references to text elsewhere.
-- To recognize those, use `id_if_reference`.
id_if_id :: String -> Maybe String
id_if_id s = head <$> matchRegex regex s
  where regex = mkRegex "^ *:ID: *([0-9a-f-]+) *$"

-- | This identifies a passage that refers to a different passage.
-- To determine which passage an ID refers to, use `id_if_id`.
id_if_reference :: String -> Maybe String
id_if_reference s = head <$> matchRegex regex s
  -- TODO: How can I make this regex as small as possible,
  -- so that it doesn't read two adjacent references as one?
  where regex = mkRegex "\\[\\[id:(.*)\\]\\[.*\\]\\]"

is_title :: String -> Bool
is_title = isJust . matchRegex (mkRegex "^ *#\\+title:")
