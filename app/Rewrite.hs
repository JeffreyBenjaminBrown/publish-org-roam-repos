module Rewrite where


import qualified Data.Map as M
import           System.FilePath (combine) -- ^ concatentate paths

import Anchor
import Types


index_and_link_to_text :: Index -> Link -> Either String String
index_and_link_to_text idx l@(Link uri _) = do
  case M.lookup uri idx of
    Nothing          -> Left  $ uri ++ " not found in index."
    Just (n :: Node) -> Right $ joinLinkText n l

joinLinkText :: Node -- ^ Should share a URI with the Link.
             -> Link -- ^ Should share a URI with the Node.
             -> String
joinLinkText n (Link _ name) =
  let r = node_repo n
      p = node_file n
      suffix = case node_headline n of
        Nothing -> ""
        Just h  -> '#' : headline_to_anchor h
  in concat
     [ "[["
     , foldl1 combine [ (repo_online_path r)
                      , "blob/master"
                      , p ]
     , suffix
     , "]["
     , name
     , "]]" ]
