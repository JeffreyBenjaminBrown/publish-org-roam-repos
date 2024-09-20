module Rewrite where


import qualified Data.List as L
import qualified Data.Map as M
import           System.FilePath (combine) -- ^ concatentate paths

import Types


-- | PITFALL: The result will differ from the input file
-- in a few ways. As intended, links will be rewritten.
-- But leading and trailing space in :PROPERTIES: drawers
-- will be lost in PROPERTIES, END and ID lines,
-- and the #+title will always be lowercase,
-- whereas occasionally it is all caps in my original data.
rewrite_file :: M.Map URI Node -> [Line] -> String
rewrite_file idx = L.intercalate "\n" . map f where
  f :: Line -> String
  f Line_PropsStart = ":PROPERTIES:"
  f Line_PropsEnd   = ":END:"
  f (Line_URI uri)  = ":ID:" ++ replicate 7 ' ' ++ uri
  f (Line_Title t)  = "#+title:" ++ t
  f (Line_Headline (Headline n nts)) =
    replicate n '*' ++ " " ++ normalTexts_to_string idx nts
  f (Line_Body nts) =         normalTexts_to_string idx nts


-- * INTERNAL
-- The rest of this is used only above and in tests.

normalTexts_to_string ::
  M.Map URI Node -> [NormalText] -> String
normalTexts_to_string idx = let
  f :: NormalText -> String
  f (NormalText_text s) = s
  f (NormalText_link link@(Link uri name)) =
     case M.lookup uri idx of
       Just node -> joinLinkText node link
       Nothing -> "[[:id:" ++ uri ++ "][" ++ name
                  ++ " -- broken link?]]"
  in concatMap f

joinLinkText :: Node -- ^ Should share a URI with the Link.
             -> Link -- ^ Should share a URI with the Node.
             -> String
joinLinkText n (Link _ name) =
  let r = node_repo n
      p = node_file n
      suffix = maybe "" ("#" ++) $ node_anchor n
  in concat
     [ "[["
     , foldl1 combine [ (repo_online_destination r)
                      , "blob/master"
                      , p ]
     , suffix
     , "]["
     , name
     , "]]" ]
