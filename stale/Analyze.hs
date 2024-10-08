module Analyze where

import Data.Maybe

import Analyze.OneLine
import Types


countTitleLine :: [String] -> Maybe Int
countTitleLine file =
  go 1 file
  where go :: Int -> [String] -> Maybe Int
        go _ [] = Nothing
        go i (line:more) = if is_title line
                           then Just i
                           else go (i+1) more

linesOfIds :: Int -> [String] -> [(URI, Maybe Int)]
linesOfIds titleLine file = let
  fromJust_and_maybeInt :: (Maybe String, Int)
                        -> (String, Maybe Int)
  fromJust_and_maybeInt (Just s,i) =
    (s, if i > titleLine
        then Just i    -- the URI is for a headline *within* the file
        else Nothing ) -- the URI is for the *entire* file
  fromJust_and_maybeInt _ =
    undefined -- no other pattern for the _ should be possible
  in map fromJust_and_maybeInt
     $ filter (isJust . fst)
     $ zip (map id_if_id $ file) [1..]
