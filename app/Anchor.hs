{- | To the best of my knowledge,
This is how Github converts headline text into an anchor.

In serial, they:
  - Strip all leading and trailing space.
  - Delete any isolated pair of dashes,
    but not if they are part of a longer string of dashes.
  - Throw away all special characters,
    keeping only alphanum, space, dash and underscore.
  - Convert each space into a dash.
  - If needed, tack on "-n" for some value of n, starting at 1,
    to distinguish it from anchors earlier in the file.
-}

module Anchor where

import Data.Char (isAlphaNum, toLower)

import Types
import Util (strip)


normalTexts_to_anchor ::
  [NormalText] -- ^ a line from an org-file
  -> String
normalTexts_to_anchor = mangleAnchorPunctuation
                        . normalTexts_to_visibleText

-- | This produces the string of characters that a human
-- would see when viewing the line in org-mode --
-- which, conveniently, is what Github uses to build an anchor.
normalTexts_to_visibleText ::
  [NormalText] -- ^ a line from an org-file
  -> String
normalTexts_to_visibleText =
  let f :: NormalText -> String
      f (NormalText_text t) = t
      f (NormalText_link (Link _ name)) = name
  in strip . concatMap f

-- | This should turn the text of a headline
-- into the anchor Github creates for it
-- (except for the dash-number that might be tacked onto the end,
-- which requires knowing the file's previous headlines).
mangleAnchorPunctuation :: String -> String
mangleAnchorPunctuation = let
  lower_and_change_spaces c = if c == ' '
                              then '-'
                              else toLower c
  should_be_kept c = -- keep alphanum, dash, underscore, space
    isAlphaNum c || c `elem` "-_ "
  in map lower_and_change_spaces
     . filter should_be_kept
     . replaceDoubleDash

-- | Github seems to strip any instance of "--"
-- from a headline before turning it into an anchor,
-- if and only if the double-dash is not part of
-- a larger group of dashes.
replaceDoubleDash :: String -> String
replaceDoubleDash "" = ""
replaceDoubleDash input =
  case takeWhile (== '-') input of
    ""     -> head input : replaceDoubleDash (tail input)
    dashes -> case length dashes of
      2 -> replaceDoubleDash $ drop 2 input
      _ -> dashes ++ ( replaceDoubleDash $
                       drop (length dashes) input )
{- -- If I could use Text.Regex.PCRE, I would use regular expressions.
   -- (Lookahead and lookbehind are not supported
   -- in the regular expression libraries I've gotten to work.)
  let match =
        {- A dash in brackets in a regular expression does not actually need escaping if it's first or last, because in those two cases it's clear that it's not intended to indicate a range. But it seems like good practice to me. -}
        "(?<=^|[^-])--(?=[^-]|$)"
      replacement = ""
  in subRegex (mkRegex match) input replacement -}
