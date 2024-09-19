module Rewrite where


import System.FilePath (combine) -- ^ concatentate paths

import Types


joinLinkText :: Repo
             -> FilePath -- ^ relative filepath in repo
             -> Link -> String
joinLinkText r p (Link _ name) =

joinLinkText :: Node -- ^ Should share a URI with the Link.
             -> Link -- ^ Should share a URI with the Node.
             -> String
joinLinkText n (Link _ name) =
  let r = node_repo n
      p = node_file n
      suffix = case node_headline n of
        Nothing               -> ""
        Just (Headline _ nts) -> undefined -- TODO
  in
  "[[" ++ foldl1 combine [ (repo_online_path r)
                         , "blob/master"
                         , p ]
  ++ "][" ++ name ++ "]]"
