module Analyze where

import           Data.Maybe
import qualified Data.Text as T
import           Text.Regex


analyze_science :: IO ()
analyze_science = do
  content <- map is_properties . lines
             <$> readFile "data/science.org"
  mapM_ (putStrLn . show) content

is_properties :: String -> Bool
is_properties =
  isJust . matchRegex (mkRegex "^ *:PROPERTIES: *$")
