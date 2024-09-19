{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import System.Directory (listDirectory)


-- All the external imports used anywhere in this repo.
-- To update, run this at the command line:
--   grep -oP '^import *\s+\K[A-Z][A-Za-z_\.]*' -r . | grep ":.*" -o | sort | uniq
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Void
import           Data.Char (isSpace,isAlphaNum, toLower)
import           Data.Maybe
import qualified Data.Map as M
import           Text.Regex
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           Text.Regex.TDFA ((=~))

import GetPaths
import GPT
import BuildIndex
import Parse
import Rewrite
import Test
import Types


thisProject :: FilePath
thisProject = "/home/jeff/org-roam/org-to-linked-md"

org_roam_dir :: String
org_roam_dir = "/home/jeff/ugh/une/org-roam"

repo_names :: [String]
repo_names = ["pers","tech","ofiscal","stale"]

main :: IO ()
main = do
    files <- listDirectory "/home/jeff/ugh/une/org-roam"
    print files
