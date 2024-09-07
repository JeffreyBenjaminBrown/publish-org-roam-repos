module Types where


-- * In use

type URI = String


-- * Not in use

data Repo = Repo {
  name  ::  String,
  root  ::  FilePath }

data Node = Node {
  uri  :: URI,
  file :: FilePath,
  line :: Maybe Int -- ^ Nothing for the URI of the whole file,
                    -- Just for URI of a heading within it
  }
