module Analyze where

import           Data.Maybe
import qualified Data.Text as T
import           Text.Regex


analyze_science :: IO ()
analyze_science = do
  file <- readFile "data/science.org"
  putStrLn "=== properties ==="
  mapM_ (putStrLn . show . is_properties) $ lines file
  putStrLn "=== ids ==="
  mapM_ (putStrLn . show . is_id)         $ lines file

is_properties :: String -> Bool
is_properties =
  isJust . matchRegex (mkRegex "^ *:PROPERTIES: *$")

is_id :: String -> Maybe String
is_id s = head <$> matchRegex regex s
  where regex = mkRegex "^ *:ID: *([0-9a-f-]+) *$"
