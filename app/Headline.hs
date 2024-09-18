module Headline where

import           Data.Maybe
import           Text.Regex


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
