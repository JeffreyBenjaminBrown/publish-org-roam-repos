module BuildIndex where

import qualified Data.Map as M

import Anchor
import Types


indexFile :: Repo -> FilePath -> [Line] -> [Node]
indexFile repo filepath the_lines =
  let
    pn = Node -- ^ Prototype for Nodes created in `go`
         { node_repo   = repo
         , node_file   = filepath
         , node_uri    = undefined
         , node_anchor = undefined }

    go :: [Line] -> Indexer -> [Node] -> [Node]
    go [] _ nodes = nodes
    go (Line_PropsStart:rest) idx nodes =
      let idx' = idx {in_props_drawer = True}
      in go rest idx' nodes
    go (Line_PropsEnd:rest) idx nodes =
      let idx' = idx {in_props_drawer = False}
      in go rest idx' nodes
    go (Line_URI uri:rest) idx nodes =
      case file_uri idx of
        Nothing -> let -- define the URI of the file, probably
          n = pn { node_uri = uri,
                   node_anchor = Nothing }
          nodes' = -- This check seems necessary but harmless.
            if in_props_drawer idx
            then n:nodes
            else nodes
          idx' = idx { file_uri = Just uri }
          in go rest idx' nodes'
        Just _ -> let -- define the URI of a headline, probably
          n = pn { node_uri = uri,
                   node_anchor = last_anchor idx }
          nodes' = if in_props_drawer idx
                   then n:nodes
                   else nodes
          in go rest idx nodes'
    go (Line_Headline h:rest) idx nodes =
      let idx' = updateIndexer h idx
      in go rest idx' nodes
    go (Line_Title _:rest) idx nodes =
      go rest idx nodes
    go (Line_Body _:rest) idx nodes =
      go rest idx nodes

  in reverse $ go the_lines initialIndexer []


-- * INTERNAL
-- The rest of this is used only above and in tests.

-- | The state of the indexer.
data Indexer = Indexer {
  in_props_drawer  :: Bool,
  title_has_passed :: Bool,
  file_uri         :: Maybe String,
  last_anchor      :: Maybe Anchor,
  anchors :: M.Map Anchor Int -- ^ Counts instances of each anchor in the file. If an anchor has already appeared earlier in the file, it will have "-n" appended, for some value of n, starting at 1.
  }

initialIndexer :: Indexer
initialIndexer = Indexer {
  in_props_drawer  = False,
  title_has_passed = False,
  file_uri         = Nothing,
  anchors          = mempty,
  last_anchor      = Nothing }

-- | Update @anchors@ and @last_anchor@
-- upon encountering a new @Headline@.
updateIndexer :: Headline -> Indexer -> Indexer
updateIndexer h idx = let
  a = headline_to_anchor h -- This anchor might need a suffix.
  (anchors', a') = case M.lookup a $ anchors idx of
     -- TODO: Will this recursively explode?
    Nothing -> ( M.insert a 1     $ anchors idx
               , a )
    Just n  -> ( M.insert a (n+1) $ anchors idx
               , a ++ "-" ++ show n )
  in idx { anchors = anchors'
         , last_anchor = Just a' }
