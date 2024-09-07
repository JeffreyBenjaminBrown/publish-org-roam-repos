module Analyze where

import           Data.Maybe
import qualified Data.Text as T
import           Text.Regex

import Types


analyze_science_file :: IO ()
analyze_science_file = do
  file <- lines <$> readFile "data/science.org"
  putStrLn "=== properties ==="
  mapM_ (putStrLn . show . is_properties) $ file
  putStrLn "=== ids ==="
  mapM_ (putStrLn . show . id_if_id)      $ file
  let Just titleLine = countTitleLine file
  putStrLn $ "=== titleLine: " ++ show titleLine
  putStrLn "=== linesOfIds ==="
  mapM_ (putStrLn . show) $ linesOfIds titleLine file

is_properties :: String -> Bool
is_properties =
  isJust . matchRegex (mkRegex "^ *:PROPERTIES: *$")

id_if_id :: String -> Maybe String
id_if_id s = head <$> matchRegex regex s
  where regex = mkRegex "^ *:ID: *([0-9a-f-]+) *$"

is_title :: String -> Bool
is_title = isJust . matchRegex (mkRegex "^ *#\\+title:")

countTitleLine :: [String] -> Maybe Int
countTitleLine lines =
  go 1 lines
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
        then Just i    -- the URI is for a heading *within* the file
        else Nothing ) -- the URI is for the *entire* file
  in map fromJust_and_maybeInt
     $ filter (isJust . fst)
     $ zip (map id_if_id $ file) [1..]
