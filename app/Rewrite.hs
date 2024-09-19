module Rewrite where


import Data.Char (isAlphaNum, toLower)
import System.FilePath (combine) -- ^ concatentate paths

import Types


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

joinLinkText :: Repo
             -> FilePath -- ^ relative filepath in repo
             -> Link -> String
joinLinkText r p (Link _ name) =
  "[[" ++ foldl1 combine [ (repo_online_path r)
                         , "blob/master"
                         , p ]
  ++ "][" ++ name ++ "]]"
