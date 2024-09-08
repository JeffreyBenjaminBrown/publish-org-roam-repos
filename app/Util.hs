module Util where

import qualified Data.Text as T


strip :: String -> String
strip = T.unpack . T.strip . T.pack
