-- | Simple parsers that look at a single line of a file,
-- or maybe even less.

module Analyze.OneLine where

import           Data.Maybe
import           Text.Regex

import Types
import Util (strip)


headline_to_anchor :: String -> String
headline_to_anchor = let
  disappears :: Char -> Bool -- Punctuation disappears.
  disappears c = isJust $ matchRegex re [c]
    where re = mkRegex "[,;\\.:\"]"

  is_alphanum :: Char -> Bool -- Special characters become dashes.
  is_alphanum c = isJust $ matchRegex re [c]
    -- TODO: This does not include Latin characters like ñóö
    where re = mkRegex "[a-zA-Z0-9]"

  in map (\c -> if is_alphanum c
                then c else '-')
     . filter (not . disappears)

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
