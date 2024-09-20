{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

-- All the external imports used anywhere in this repo.
-- To update, run this at the command line:
--   grep -oP '^import *\s+\K[A-Z][A-Za-z_\.]*' -r . | grep ":.*" -o | sort | uniq
import           Control.Monad (foldM)
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

import Config
import GetPaths
import GPT
import BuildIndex
import Parse
import Rewrite
import Test
import Types


-- | TODO: Parsing the file twice is inefficient.
-- (@indexRepos@ and @rewrite_file@ both use @parseFile@.)
main :: IO ([MPError],Index)
main = do
  (errs, idx) <- indexRepos $ M.elems repos
  let whole_files :: [Node] -- as opposed to headlines
      whole_files = filter (isNothing . node_anchor) $ M.elems idx
  mapM_ (rewrite_file idx) $ whole_files
  return (errs,idx)
