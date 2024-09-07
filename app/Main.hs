module Main where

import System.Directory (listDirectory)

import Analyze
import GetPaths
import ReadAndWrite
import Text
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
