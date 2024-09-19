module Rewrite where


import System.FilePath (combine) -- ^ concatentate paths

import Types


joinLinkText :: Repo
             -> FilePath -- ^ relative filepath in repo
             -> Link -> String
joinLinkText r p (Link _ name) =
  "[[" ++ foldl1 combine [ (repo_online_path r)
                         , "blob/master"
                         , p ]
  ++ "][" ++ name ++ "]]"
