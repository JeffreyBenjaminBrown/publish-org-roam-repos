module Rewrite where

import System.FilePath (combine) -- ^ to concatentate paths

import Types


joinLinkText :: Repo -> FilePath -> Link -> String
joinLinkText r p (Link _ name) =
  "[[" ++ foldl1 combine [ (repo_online_path r)
                         , "blob/master"
                         , p ]
  ++ "][" ++ name ++ "]]"