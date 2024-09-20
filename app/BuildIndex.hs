module BuildIndex where

import           Control.Monad (foldM)
import qualified Data.Map as M
import           System.FilePath (combine) -- to join paths

import Anchor
import GetPaths
import Parse (parseFile)
import Types


indexRepos :: [Repo] -> IO ([MPError], Index)
indexRepos repos = foldM addRepoToIndex mempty repos

addRepoToIndex ::    ([MPError], Index)
               -> Repo
               -> IO ([MPError], Index)
addRepoToIndex acc repo = do
  relativePaths :: [FilePath] <-
    relFfilesAnyDepth ".org" $ repo_local_path repo
  let doFile :: ([MPError],Index)
             -> FilePath
             -> IO          ([MPError],Index)
      doFile acc relpath = addFileToIndex repo relpath acc
  foldM doFile acc relativePaths

addFileToIndex :: Repo
               -> FilePath -- ^ relative to the repo
               ->    ([MPError], Index)
               -> IO ([MPError], Index)
addFileToIndex repo filepath (errs, idx) = do
  e_lines :: Either MPError [Line] <-
    parseFile $ combine (repo_local_path repo) filepath
  case e_lines of
    Left err -> return (err:errs,idx)
    Right (the_lines :: [Line]) -> do
      let unodes :: [(URI,Node)] =
            indexFile repo filepath the_lines
          myInsert :: (URI,Node) -> Index -> Index
          myInsert (u,n) = M.insert u n
          in return (errs, foldr myInsert idx unodes)

indexFile :: Repo -> FilePath -> [Line] -> [(URI,Node)]
indexFile repo filepath the_lines =
  let
    pn = Node -- ^ Prototype for Nodes created in `go`
         { node_repo   = repo
         , node_file   = filepath
         , node_anchor = undefined }

    go :: [Line] -> Indexer -> [(URI,Node)] -> [(URI,Node)]
    go [] _ unodes = unodes
    go (Line_PropsStart:rest) idx unodes =
      let idx' = idx {in_props_drawer = True}
      in go rest idx' unodes
    go (Line_PropsEnd:rest) idx unodes =
      let idx' = idx {in_props_drawer = False}
      in go rest idx' unodes
    go (Line_URI uri:rest) idx unodes =
      case file_uri idx of
        Nothing -> let -- define the URI of the file, probably
          n = pn { node_anchor = Nothing }
          unodes' = -- This check seems necessary but harmless.
            if in_props_drawer idx
            then (uri,n):unodes
            else unodes
          idx' = idx { file_uri = Just uri }
          in go rest idx' unodes'
        Just _ -> let -- define the URI of a headline, probably
          n = pn { node_anchor = last_anchor idx }
          unodes' = if in_props_drawer idx
                    then (uri,n):unodes
                    else unodes
          in go rest idx unodes'
    go (Line_Headline h:rest) idx unodes =
      let idx' = updateIndexer h idx
      in go rest idx' unodes
    go (Line_Title _:rest) idx unodes =
      go rest idx unodes
    go (Line_Body _:rest) idx unodes =
      go rest idx unodes

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
