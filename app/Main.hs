{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import System.Directory (listDirectory)


-- All the external imports used anywhere in this repo.
-- To update, run this at the command line:
--   grep -oP '^import *\s+\K[A-Z][A-Za-z_\.]*' -r . | grep ":.*" -o | sort | uniq
import           Data.Char (isSpace,isAlphaNum, toLower)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Void
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Regex
import           Text.Regex.TDFA ((=~))

import GetPaths
import GPT
import BuildIndex
import Parse
import Rewrite
import Test
import Types


main :: IO ()
main = do
    print "hello"
