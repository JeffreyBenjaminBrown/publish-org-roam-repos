module BuildIndex where

import Types


-- | The state of the indexer.
data Indexer = Indexer {
  in_props_drawer :: Bool,
  title_has_passed :: Bool,
  file_uri :: Maybe String,
  last_headline_line_number :: Maybe Int }

initialIndexer :: Indexer
initialIndexer = Indexer {
  in_props_drawer = False,
  title_has_passed = False,
  file_uri = Nothing,
  last_headline_line_number = Nothing }

indexFile :: Repo -> FilePath -> [(Int,Line)] -> [Node]
indexFile repo filepath the_lines =
  let
    pn = Node -- ^ Prototype for Nodes created in `go`
         { node_repo = repo
         , node_file = filepath
         , node_uri  = undefined
         , node_line = undefined }

    go :: [(Int,Line)] -> Indexer -> [Node] -> [Node]
    go [] _ nodes = nodes
    go ((_,Line_PropsStart):rest) idx nodes =
      let idx' = idx {in_props_drawer = True}
      in go rest idx' nodes
    go ((_,Line_PropsEnd):rest) idx nodes =
      let idx' = idx {in_props_drawer = False}
      in go rest idx' nodes
    go ((_,Line_URI uri):rest) idx nodes =
      case file_uri idx of
        Nothing -> let -- define the URI of the file, probably
          n = pn { node_uri = uri,
                   node_line = Nothing }
          nodes' = -- This check seems necessary but harmless.
            if in_props_drawer idx
            then n:nodes
            else nodes
          idx' = idx { file_uri = Just uri }
          in go rest idx' nodes'
        Just _ -> let -- define the URI of a heading, probably
          n = pn { node_uri = uri,
                   node_line = last_headline_line_number idx }
          nodes' = if in_props_drawer idx
                   then n:nodes
                   else nodes
          in go rest idx nodes'
    go ((n,Line_Headline _ _):rest) idx nodes =
      let idx' = idx { last_headline_line_number = Just n }
      in go rest idx' nodes
    go ((_,Line_Title _):rest) idx nodes =
      go rest idx nodes
    go ((_,Line_Body _):rest) idx nodes =
      go rest idx nodes

  in go the_lines initialIndexer []