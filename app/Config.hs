module Config where

import qualified Data.Map as M
import           System.FilePath ((</>)) -- to join paths

import Types


-- | Only used for tests.
this_repo :: FilePath
this_repo = "/home/jeff/code/org-roam-to-org-git"

-- | The folder with your original org-roam repos.
offlinePrefix_source :: FilePath
offlinePrefix_source = "/home/jeff/ugh/une/org-roam"

-- | The folder with the destination repos on your computer.
offlinePrefix_destination :: FilePath
offlinePrefix_destination =
  "/home/jeff/ugh/une/github-exported-org-roam"

-- | Where the destination repos will end up online.
-- (If not Github, the links might not be navigable.)
onlinePrefix :: FilePath
onlinePrefix = "https://github.com/JeffreyBenjaminBrown"

-- | The first item in each of these pairs
-- is not actually used by the code;
-- it's just to help you keep track of things.
repos :: M.Map String Repo
repos = M.fromList
  [ ( "personal"
    , Repo { repo_local_source  =
             offlinePrefix_source </> "pers"
           , repo_local_destination =
             offlinePrefix_destination </> "pers"
           , repo_online_destination = onlinePrefix </>
             "secret_org_with_github-navigable_links" } )

  , ( "public"
    , Repo { repo_local_source  =
             offlinePrefix_source </> "tech"
           , repo_local_destination =
             offlinePrefix_destination </> "tech"
           , repo_online_destination =
             onlinePrefix </>
             "public_notes_with_github-navigable_links" } )

  , ( "observatorio",
      Repo { repo_local_source  =
             offlinePrefix_source </> "ofiscal"
           , repo_local_destination =
             offlinePrefix_destination </> "ofiscal"
           , repo_online_destination =
             onlinePrefix </>
             "knowledge_graph_with_github-navigable_links" } )

  , ( "stale",
      Repo { repo_local_source  =
             offlinePrefix_source </> "stale"
           , repo_local_destination =
             offlinePrefix_destination </> "stale"
           , repo_online_destination =
             onlinePrefix </>
             "stale_notes_with_github-navigable_links" } ) ]
